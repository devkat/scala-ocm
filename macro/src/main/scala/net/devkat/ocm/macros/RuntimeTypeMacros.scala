package net.devkat.ocm.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.annotation.StaticAnnotation
import com.typesafe.scalalogging.slf4j.Logging
import java.util.Calendar
import scala.tools.reflect.Eval

import net.devkat.ocm.PropertyType

class nodeRuntime extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro RuntimeOcmMacro.node_impl
}

class propertyRuntime(val propertyType: PropertyType[_] = null) extends StaticAnnotation

/*
 * https://github.com/adamw/scala-macro-aop/blob/master/macros/src/main/scala/com/softwaremill/aop/delegateMacro.scala
 */
object RuntimeOcmMacro extends Logging {

  def getType[T](c: Context)(x: c.Expr[T]): Type = {
    // creates a runtime reflection universe to host runtime compilation
    import scala.reflect.runtime.{ universe => ru }
    //val mirror = ru.runtimeMirror(c.libraryClassLoader)
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    import scala.tools.reflect.ToolBox
    val toolBox = mirror.mkToolBox()

    // runtime reflection universe and compile-time macro universe are different
    // therefore an importer is needed to bridge them
    // currently mkImporter requires a cast to correctly assign the path-dependent types
    val importer0 = ru.mkImporter(c.universe)
    val importer = importer0.asInstanceOf[ru.Importer { val from: c.universe.type }]

    // the created importer is used to turn a compiler tree into a runtime compiler tree
    // both compilers use the same classpath, so semantics remains intact
    val imported:Tree = importer.importTree(x.tree)

    // after the tree is imported, it can be evaluated as usual
    val tree = toolBox.resetAllAttrs(imported.duplicate)
    toolBox.eval(q"scala.reflect.runtime.universe.typeOf[$imported]").asInstanceOf[Type]
  }
  
  def reportInvalidAnnotationTarget(c: Context)(t: Any) {
    c.error(c.enclosingPosition, s"This annotation can't be used on ${t}.")
  }

  def node_impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def getPropertyAnnotation(field: ValDef) =
      field.mods.annotations.find(_.find(t => (t.tpe != null) && (t.tpe =:= typeOf[property])).isDefined)

    def dump(t: Tree): String = {
      s"""[${t.getClass.getName}] ${t.toString} (${t.children.map(dump _).mkString(", ")})"""
    }

    lazy val type2propertyType_string = Map(
      "Array[Byte]" -> "binary",
      "BigDecimal" -> "decimal",
      "Calendar" -> "date",
      "Double" -> "double",
      "Long" -> "long",
      "String" -> "string")

    lazy val type2propertyType = {
      import PropertyType._
      import scala.reflect.runtime.universe._
      Map[Type, PropertyType[_]](
        typeOf[Array[Byte]] -> binary,
        typeOf[BigDecimal] -> decimal,
        typeOf[Calendar] -> date,
        typeOf[Double] -> double,
        typeOf[Long] -> long,
        typeOf[String] -> string)
    }

    def defaultPropType(t: scala.reflect.runtime.universe.Type): Option[PropertyType[_]] =
      type2propertyType.find { case (t2, pt) => t =:= t2 }.map(_._2)

    lazy val container2prefix = Map(
      "Option" -> "optional",
      "Iterable" -> "multi")

    def defaultPropertyType(varType: scala.reflect.runtime.universe.Type): Tree = {
      import PropertyType._
      def invalid = {
        c.error(c.enclosingPosition, s"Invalid property type ${showRaw(varType)}")
        EmptyTree
      }
      def propType(name: String): Tree =
        Select(Ident(reify(PropertyType).tree.symbol), newTermName(name))

      defaultPropType(varType).map(t => {
        val symbol = t.asInstanceOf[TypeRef].pre.typeSymbol
        q"$symbol"
      }).getOrElse(invalid)
      
      //println("TREE: " + showRaw(reify(string).tree))
      // Select(Ident(net.devkat.ocm.PropertyType), newTermName("string"))
      /*
      varType match {
        case Ident(typeName) => {
          val typeSym = c.mirror.staticClass(varType.)
          defaultPropType(typeSym.toType).map(t => {
            val symbol = c.mirror.staticModule(t.getClass.getName)
            q"$symbol"
          }).getOrElse(invalid)
        }
        case AppliedTypeTree(Ident(containerTypeName), List(Ident(baseTypeName))) if List("Option", "Iterable").contains(containerTypeName.decoded) => {
          type2propertyType.get(baseTypeName.decoded).map(n => propType(container2prefix(containerTypeName.decoded) + n)).getOrElse(invalid)
        }
        case AppliedTypeTree(Ident(containerType), List(Ident(baseType))) if List("Option", "List").contains(containerType.decoded) => {
          containerType.decoded match {
            case "Option" => baseType 
          }
        }
        case _ => invalid
      }
       */
    }

    def addAccessors(classDef: ClassDef) = {
      val ClassDef(mods, className, tparams, Template(parents, self, body)) = classDef

      val newBody = body map { tree =>
        tree match {
          case field @ ValDef(mods, name, typeTree, rhs) => {
            val plainFieldName = name.decoded
            val varName = newTermName("_" + name.decoded)
            val runtimeVarType = getType(c)(c.Expr[Type](field.tpt))
            logger.info(s"Type of ${plainFieldName}: ${typeTree}")
            val setterName = newTermName(name.decoded + "_$eq")
            val nameLiteral = Literal(Constant(plainFieldName))

            /*
            println(plainFieldName + " =>\n  " + field.mods.annotations.map( a =>
              a.symbol//dump(a)
              ).mkString("\n  ") )
            */

            /*
            val ann = field.mods.annotations.map{
              _.collect{
                case a @ Apply(Select(New(Ident(typeName)), nme.CONSTRUCTOR), params) if typeName.decoded == "property" => {
                  params collect {
                    case AssignOrNamedArg(Ident(paramName), Literal(Constant(paramValue))) => (paramName.decoded, paramValue)
                  }
                }
              }
            }.headOption
            */
            val ann = mods.annotations.collect {
              case Apply(Select(New(Ident(typeName)), nme.CONSTRUCTOR), paramTrees) if typeName.decoded == "property" => {
                paramTrees.collect {
                  case AssignOrNamedArg(Ident(paramName), paramValue) => (paramName.decoded, paramValue)
                }.toMap
              }
            }.headOption

            //lazy val defaultPropertyType = q"PropertyType.defaultPropertyType[$varType]"
            /*
            lazy val defaultPropertyType = {
              val t = PropertyType.defaultPropertyType()(TypeTag.)
              reify(t)
            }
            */
            lazy val defaultType = defaultPropertyType(runtimeVarType)

            val propertyType: Tree = ann match {
              case Some(params) => params.get("propertyType").getOrElse(defaultType)
              case None => defaultType
            }

            logger.info(s"Mapping field ${className}.${plainFieldName} to ${propertyType} property '${plainFieldName}'.")

            List(
              //q"private var $varName:$varType = _",
              q"""def $name = readProperty[$typeTree](this, ${nameLiteral}, ${propertyType})""",
              //q"def $setterName(a: $varType) = $varName = a"
              q"""def $setterName(a: $typeTree) {
                println("Setting variable to %s".format(a))
                writeProperty[$typeTree](this, ${nameLiteral}, ${propertyType}, a)
              }"""): List[Tree]
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