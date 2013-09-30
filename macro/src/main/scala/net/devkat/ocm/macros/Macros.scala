package net.devkat.ocm.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.StaticAnnotation

class node extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro OcmMacro.node_impl
}

class property extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro OcmMacro.property_impl
}

/*
 * https://github.com/adamw/scala-macro-aop/blob/master/macros/src/main/scala/com/softwaremill/aop/delegateMacro.scala
 */
object OcmMacro {

  def reportInvalidAnnotationTarget(c: Context)(t: Any) {
    c.error(c.enclosingPosition, s"This annotation can't be used on ${t}.")
  }

  def node_impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def addAccessors(classDef: ClassDef) = {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = classDef

      val newBody = body map { tree =>
        tree match {
          case field: ValDef => {
            val fieldName = field.name
            val varName = newTermName("_" + fieldName.decoded)
            val varType = field.tpt
            val setterName = newTermName(fieldName.decoded + "_=")
            List(
              q"private var $varName:$varType = _",
              q"def $fieldName = $varName",
              //q"def $setterName(a: $varType) = $varName = a"
              q"""def $setterName(a: $varType) { println("Setting variable to %s".format(a)); $varName = a }""",
              q"""def test(a: $varType) { println("Setting variable to %s".format(a)); $varName = a }"""
              ): List[Tree]
          }
          case t => List(t)
        }
      } flatten

      ClassDef(mods, name, tparams, Template(parents, self, newBody))
    }

    val inputs = annottees.map(_.tree).toList
    val outputs: List[Tree] = inputs match {
      case (classDef: ClassDef) :: rest => addAccessors(classDef) :: rest
      case t => reportInvalidAnnotationTarget(c)(t); EmptyTree :: Nil
    }
    c.Expr[Any](Block(outputs, Literal(Constant(()))))

  }

  def property_impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    // From MacWire ...
    def typeCheckExpressionOfType(typeTree: Tree): Type = {
      val someValueOfTypeString = reify {
        def x[T](): T = throw new Exception
        x[String]()
      }
      val Expr(Block(stats, Apply(TypeApply(someValueFun, _), someTypeArgs))) = someValueOfTypeString
      val someValueOfGivenType = Block(stats, Apply(TypeApply(someValueFun, List(typeTree)), someTypeArgs))
      val someValueOfGivenTypeChecked = c.typeCheck(someValueOfGivenType)
      someValueOfGivenTypeChecked.tpe
    }

    def computeType(tpt: Tree): Type = {
      if (tpt.tpe != null) {
        tpt.tpe
      } else {
        val calculatedType = c.typeCheck(tpt.duplicate, silent = true, withMacrosDisabled = true).tpe
        val result = if (tpt.tpe == null) calculatedType else tpt.tpe
        if (result == NoType) typeCheckExpressionOfType(tpt) else result
      }
    }

    /*
    def addProxyMethods(valDef: ValDef, addToClass: ClassDef) = {
      def allMethodsInDelegate = computeType(valDef.tpt).declarations

      val ClassDef(mods, name, tparams, Template(parents, self, body)) = addToClass

      // TODO better filtering - allow overriding
      val existingMethods = body.flatMap(tree => tree match {
        case DefDef(_, n, _, _, _, _) => Some(n)
        case _ => None
      }).toSet
      val methodsToAdd = allMethodsInDelegate.filter(method => !existingMethods.contains(method.name))

      val newMethods = for {
        methodToAdd <- methodsToAdd
      } yield {
        val methodSymbol = methodToAdd.asMethod

        val vparamss = methodSymbol.paramss.map(_.map {
          paramSymbol => ValDef(
            Modifiers(Flag.PARAM, tpnme.EMPTY, List()),
            paramSymbol.name.toTermName,
            TypeTree(paramSymbol.typeSignature),
            EmptyTree)
        })

        val delegateInvocation = Apply(
          Select(Ident(valDef.name), methodSymbol.name),
          methodSymbol.paramss.flatMap(_.map(param => Ident(param.name)))) // TODO - multi params list

        DefDef(Modifiers(),
          methodSymbol.name,
          List(), // TODO - type parameters
          vparamss,
          TypeTree(methodSymbol.returnType),
          delegateInvocation)
      }

      ClassDef(mods, name, tparams, Template(parents, self, body ++ newMethods))
    }
    */

    def addAccessors(valDef: ValDef, classDef: ClassDef) = {
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = classDef

      val varName = valDef.name
      val privateVarName = newTermName("_" + varName.decoded)
      val varType = valDef.tpt
      val setterName = newTermName(varName.decoded + "_=")

      val newBody = body map { tree =>
        tree match {
          case param: ValDef if param.name == valDef.name => List(
            q"private var $privateVarName:$varType = _",
            q"def $varName = $privateVarName",
            q"def $setterName(a: $varType) = $privateVarName = a" //q"""def $setterName(a: $varType) { println("Setting variable to %s".format(a)); $privateVarName = a }"""
            ): List[Tree]
          case t => List(t)
        }
      } flatten

      newBody foreach println _

      /*
      val setter = DefDef(Modifiers(),
        newTermName("set" + valDef.name.decoded),
        List(), // TODO - type parameters
        List(List(valDef)),
        TypeTree(typeOf[Unit]),
        reify { println("here") }.tree)

      val getter = DefDef(Modifiers(),
        newTermName("get" + valDef.name.decoded),
        List(TypeDef(valDef.tpt.symbol)), // TODO - type parameters
        List(),
        TypeTree(typeOf[Unit]),
        valDef)

      println("Adding setter " + setter)
      */

      ClassDef(mods, name, tparams, Template(parents, self, newBody))
    }

    val inputs = annottees.map(_.tree).toList
    val (_, expandees) = inputs match {
      case (param: ValDef) :: (enclosing: ClassDef) :: rest => {
        val newEnclosing = addAccessors(param, enclosing)
        (param, newEnclosing :: rest)
      }
      case (param: TypeDef) :: (rest @ (_ :: _)) =>
        reportInvalidAnnotationTarget(c)(); (param, rest)
      case t => reportInvalidAnnotationTarget(c)(t); (EmptyTree, inputs)
    }
    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

/*
object Macros {

  def property[T](name: String): Unit = macro property_impl[T]

  def property_impl[T: c.WeakTypeTag](c: Context)(name: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](Apply(Select(c.prefix.tree, newTermName(name.splice + "_=")), List(value.tree)))
  }

}
*/