package net.devkat.ocm.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.annotation.StaticAnnotation
import com.typesafe.scalalogging.slf4j.Logging
import java.util.Calendar
import scala.tools.reflect.Eval
import scala.collection.immutable.StringOps

import net.devkat.ocm.{Cardinality, PropertyType}

class node extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro OcmMacro.node_impl
}

class property(val propertyType: PropertyType = null) extends StaticAnnotation

/*
 * https://github.com/adamw/scala-macro-aop/blob/master/macros/src/main/scala/com/softwaremill/aop/delegateMacro.scala
 */
object OcmMacro extends Logging {
  
  def runtimeExpr[T1, T2](c: Context)(x: c.Expr[T1], codeCtx: Tree => Tree): T2 = {
    import scala.tools.reflect.ToolBox
    // creates a runtime reflection universe to host runtime compilation
    import scala.reflect.runtime.{ universe => ru }
    //val mirror = ru.runtimeMirror(c.libraryClassLoader)
    val mirror = ru.runtimeMirror(getClass.getClassLoader)

    // runtime reflection universe and compile-time macro universe are different
    // therefore an importer is needed to bridge them
    // currently mkImporter requires a cast to correctly assign the path-dependent types
    val importer0 = ru.mkImporter(c.universe)
    val importer = importer0.asInstanceOf[ru.Importer { val from: c.universe.type }]

    // the created importer is used to turn a compiler tree into a runtime compiler tree
    // both compilers use the same classpath, so semantics remains intact
    val imported: Tree = importer.importTree(x.tree)

    // after the tree is imported, it can be evaluated as usual
    val toolBox = mirror.mkToolBox()
    val tree = toolBox.resetAllAttrs(imported.duplicate)
    toolBox.eval(codeCtx(imported)).asInstanceOf[T2]
  }

  def runtimeType(c: Context)(x: c.Expr[c.universe.Type]): Type = runtimeExpr(c)(x,
      imported => q"import java.util.Calendar; scala.reflect.runtime.universe.typeOf[$imported]")

  def reportInvalidAnnotationTarget(c: Context)(t: Any) {
    c.error(c.enclosingPosition, s"This annotation can't be used on ${t}.")
  }

  def node_impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def getPropertyAnnotation(field: ValDef) =
      field.mods.annotations.find(_.find(t => (t.tpe != null) && (t.tpe =:= typeOf[property])).isDefined)

    object MappableType {
      //import JcrType._
      import PropertyType._
      def unapply(t: Tree): Option[(PropertyType, Tree)] = {
        val tpe = runtimeType(c)(c.Expr[Type](t))
        (if (tpe =:= universe.typeOf[Array[Byte]]) Some((simple(binary), reify(simple(binary)))) else
        if (tpe =:= universe.typeOf[Boolean]) Some((simple(boolean), reify(simple(boolean)))) else
        if (tpe =:= universe.typeOf[BigDecimal]) Some((simple(decimal), reify(simple(decimal)))) else
        if (tpe =:= universe.typeOf[Calendar]) Some((simple(date), reify(simple(date)))) else
        if (tpe =:= universe.typeOf[Double]) Some((simple(double), reify(simple(double)))) else
        if (tpe =:= universe.typeOf[Long]) Some((simple(long), reify(simple(long)))) else
        if (tpe =:= universe.typeOf[String]) Some((simple(string)), reify(simple(string))) else
        if (tpe =:= universe.typeOf[Option[Array[Byte]]]) Some((optional(binary), reify(optional(binary)))) else
        if (tpe =:= universe.typeOf[Option[BigDecimal]]) Some((optional(decimal), reify(optional(decimal)))) else
        if (tpe =:= universe.typeOf[Option[Boolean]]) Some((optional(boolean), reify(optional(boolean)))) else
        if (tpe =:= universe.typeOf[Option[Calendar]]) Some((optional(date), reify(optional(date)))) else
        if (tpe =:= universe.typeOf[Option[Double]]) Some((optional(double), reify(optional(double)))) else
        if (tpe =:= universe.typeOf[Option[Long]]) Some((optional(long), reify(optional(long)))) else
        if (tpe =:= universe.typeOf[Option[String]]) Some((optional(string), reify(optional(string)))) else
        if (tpe <:< universe.typeOf[Iterable[Array[Byte]]]) Some((multi(binary), reify(multi(binary)))) else
        if (tpe <:< universe.typeOf[Iterable[BigDecimal]]) Some((multi(decimal), reify(multi(decimal)))) else
        if (tpe <:< universe.typeOf[Iterable[Boolean]]) Some((multi(boolean), reify(multi(boolean)))) else
        if (tpe <:< universe.typeOf[Iterable[Calendar]]) Some((multi(date), reify(multi(date)))) else
        if (tpe <:< universe.typeOf[Iterable[Double]]) Some((multi(double), reify(multi(double)))) else
        if (tpe <:< universe.typeOf[Iterable[Long]]) Some((multi(long), reify(multi(long)))) else
        if (tpe <:< universe.typeOf[Iterable[String]]) Some((multi(string), reify(multi(string)))) else
        None).map { case (t, expr) => (t, expr.tree) }
      }
    }
      
    /*
    def propType(name: String) =
      Select(Ident(reify(PropertyType).tree.symbol), newTermName(name))

    object SimpleType {
      def unapply(t: Tree): Option[String] = t match {
        case Ident(name) => Some(name.decoded)
        case _ => None
      }
    }

    object MappableType {
      def unapply(t: Tree): Option[Tree] = {
        val tpe = runtimeType(c)(c.Expr[Type](t))
        if (tpe =:= universe.typeOf[Array[Byte]]) Some(propType("binary")) else
        if (tpe =:= universe.typeOf[Boolean]) Some(propType("boolean")) else
        if (tpe =:= universe.typeOf[BigDecimal]) Some(propType("decimal")) else
        if (tpe =:= universe.typeOf[Calendar]) Some(propType("date")) else
        if (tpe =:= universe.typeOf[Double]) Some(propType("double")) else
        if (tpe =:= universe.typeOf[Long]) Some(propType("long")) else
        if (tpe =:= universe.typeOf[String]) Some(propType("string")) else
        if (tpe =:= universe.typeOf[Option[Array[Byte]]]) Some(propType("optionalBinary")) else
        if (tpe =:= universe.typeOf[Option[BigDecimal]]) Some(propType("optionalDecimal")) else
        if (tpe =:= universe.typeOf[Option[Boolean]]) Some(propType("optionalBoolean")) else
        if (tpe =:= universe.typeOf[Option[Calendar]]) Some(propType("optionalDate")) else
        if (tpe =:= universe.typeOf[Option[Double]]) Some(propType("optionalDouble")) else
        if (tpe =:= universe.typeOf[Option[Long]]) Some(propType("optionalLong")) else
        if (tpe =:= universe.typeOf[Option[String]]) Some(propType("optionalString")) else
        if (tpe <:< universe.typeOf[Iterable[Array[Byte]]]) Some(propType("multiBinary")) else
        if (tpe <:< universe.typeOf[Iterable[BigDecimal]]) Some(propType("multiDecimal")) else
        if (tpe <:< universe.typeOf[Iterable[Boolean]]) Some(propType("multiBoolean")) else
        if (tpe <:< universe.typeOf[Iterable[Calendar]]) Some(propType("multiDate")) else
        if (tpe <:< universe.typeOf[Iterable[Double]]) Some(propType("multiDouble")) else
        if (tpe <:< universe.typeOf[Iterable[Long]]) Some(propType("multiLong")) else
        if (tpe <:< universe.typeOf[Iterable[String]]) Some(propType("multiString")) else
        None
      }
    }
*/
    /*
    object MappableType {
      def genType(baseType: Name, prefix: String): Tree =
        propType(prefix + new StringOps(baseType.decoded).capitalize)

      def unapply(t: Tree): Option[Tree] = t match {
        case AppliedTypeTree(Ident(q"Option"), List(MappableBaseType(Select(_, q"$bt")))) => Some(genType(bt, "optional"))
        case AppliedTypeTree(Ident(q"Iterable"), List(MappableBaseType(Select(_, q"$bt")))) => Some(genType(bt, "multi"))
        case MappableBaseType(bt) => Some(bt)
        case _ => None
      }
    }
    */

    def invalid(varType: Tree) = {
      c.error(c.enclosingPosition, s"Can't map property type ${show(varType)} (${showRaw(varType)})")
      EmptyTree
    }

    /**
     * Map mappable type to property type.
     */
    def mapType(t: Tree): Option[(PropertyType, Tree)] = t match {
      case MappableType(tpe) => Some(tpe)
      case _ => None
    }

    def addAccessors(classDef: ClassDef) = {
      val ClassDef(mods, className, tparams, Template(parents, self, body)) = classDef

      val newBody = body map { tree =>
        tree match {
          case field @ ValDef(mods, name, typeTree, rhs) => {
            val plainFieldName = name.decoded
            val varName = newTermName("_" + name.decoded)
            val setterName = newTermName(name.decoded + "_$eq")
            val nameLiteral = Literal(Constant(plainFieldName))

            val ann = mods.annotations.collect {
              case Apply(Select(New(Ident(typeName)), nme.CONSTRUCTOR), paramTrees) if typeName.decoded == "property" => {
                paramTrees.collect {
                  case AssignOrNamedArg(Ident(paramName), paramValue) => (paramName.decoded, paramValue)
                }.toMap
              }
            }.headOption
            
            val propertyType:Option[(PropertyType, Tree)] = ann.flatMap(_.get("propertyType")) match {
              case None => mapType(typeTree)
              /*
              case None => mapType(typeTree) match {
                case Some(t) => Some((t, reify(t).tree))
                case None => None
              }
              */
              // FIXME support propertyType annotation
              case t => None
            }
            
            propertyType match {
              case Some((propertyType, propertyTypeTree)) => {
                import Cardinality._
                logger.info(s"Mapping field ${className}.${plainFieldName} to ${propertyTypeTree} property '${plainFieldName}'.")
                val method = propertyType.cardinality match {
                  case Cardinality.Simple => "Single"
                  case Cardinality.Multi => "Multiple"
                  case Cardinality.Optional => "Optional"
                }
                /*
                def toMethod(s:String) = Literal(Constant(s + method + "Property"))
                val getter = toMethod("read")
                val setter = toMethod("write")
                */
                
                val baseType = typeTree.children match {
                  case List(_, t) => Some(t)
                  case _ => None
                }
                
                List(
                  //q"private var $varName:$varType = _",
                  propertyType.cardinality match {
                    case Simple => q"""def $name = readSingleProperty[$typeTree](this, ${nameLiteral})"""
                    case Multi => q"""def $name = readMultipleProperty[${baseType.get}](this, ${nameLiteral})"""
                    case Optional => q"""def $name = readOptionalProperty[${baseType.get}](this, ${nameLiteral})"""
                  },
                  //q"""def $name = $getter[$typeTree](this, ${nameLiteral}, ${propertyTypeTree})""",
                  //q"def $setterName(a: $varType) = $varName = a"
                  propertyType.cardinality match {
                    case Simple => q"""def $setterName(a: $typeTree) {
                      println("Setting variable to %s".format(a))
                      writeSingleProperty[$typeTree](this, ${nameLiteral}, a)
                    }"""
                    case Multi => q"""def $setterName(a: $typeTree) {
                      println("Setting variable to %s".format(a))
                      writeMultipleProperty[${baseType.get}](this, ${nameLiteral}, a)
                    }"""
                    case Optional => q"""def $setterName(a: $typeTree) {
                      println("Setting variable to %s".format(a))
                      writeOptionalProperty[${baseType.get}](this, ${nameLiteral}, a)
                    }"""
                  }
                ): List[Tree]
              }
              case None => List(invalid(typeTree))
            }

          }
          case t => List(t)
        }
      } flatten

      ClassDef(mods, className, tparams, Template(parents, self, newBody))
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