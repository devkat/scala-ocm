package net.devkat.scalaocm

import scala.reflect.runtime.universe.FieldMirror
import com.typesafe.scalalogging.slf4j.Logging
import javax.jcr.Node
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import net.devkat.scalaocm.annotation.JcrProperty

case class FieldMetaData(
  term: TermSymbol,
  valueType: Type
)

trait Mapper extends Logging {
  
  self: SessionHolder =>
  
  import Reflection._
  import ValueConversions._
    
  def setFieldValue(field: FieldMirror, value: Any): Unit

  def getFieldValue(field: FieldMirror): Any
  
  def getFieldType(field: TermSymbol): Type
  
  protected def jcrPath(node: Node) = Path.parse(node.getPath)

  protected def fromNode[T <: AnyRef](n: Node)(implicit m: Manifest[T]): T = {
    node2obj.value.getOrElseUpdate(n, {
      val r = newInstance[T]
      load(r, n)
      r
    }).asInstanceOf[T]
  }

  protected def load(r:AnyRef, node: Node) = {
    println("Loading node")
    JcrHelpers.dump(node)
    val mirror = instanceMirror(r)
    getMappedFields(mirror.symbol) foreach { field =>
      val name = fieldName(field.term)
      val value = getPropertyValue(node, name, getFieldType(field.term))
      logger.info("Loading property {}.{} := {}", node.getPath, name, value)
      //instanceMirror.reflectField(field).set(value.orNull)
      value match {
        case Left(error) => throw new RuntimeException(error)
        case Right(v) => setFieldValue(mirror.reflectField(field.term), v.value)
      }
    }
  }
  
  protected def save(r:AnyRef) = {
    withNode(r) { n =>

      def setProperty(name: String, v: Any) {
        v match {
          case null => n.setProperty(name, null.asInstanceOf[String])
          case l: List[_] => n.setProperty(name, list2values(l))
          case v => any2valueOption(v) match {
            case Some(value) => n.setProperty(name, value)
            case None => if (n.hasProperty(name)) n.getProperty(name).remove()
          }
        }
      }

      val mirror = instanceMirror(r)
      getMappedFields(mirror.symbol) foreach { field =>
        val v = getFieldValue(mirror.reflectField(field.term))
        val name = fieldName(field.term)
        setProperty(name, v)
        logger.info("Save property {}.{} = {}", jcrPath(n), name, getPropertyValue(n, name, getFieldType(field.term)))
      }
    }
  }

  protected def getMappedFields(symbol: ClassSymbol): Iterable[FieldMetaData] =
    symbol.toType.members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) &&
        t.annotations.find(_.tpe == typeOf[JcrProperty]).isDefined => FieldMetaData(t, getFieldType(t))
    }

  protected def getPropertyValue(node:Node, name: String, t: Type): Either[String, PropertyValue[_]] = {
    
    val isSimple = t <:< typeOf[SimpleValue[_]]
    val isMulti = t <:< typeOf[MultiValue[_]]
    val isOption = t <:< typeOf[OptionValue[_]]
    
    if (node.hasProperty(name)) {
      val prop = node.getProperty(name)
      if (prop.isMultiple) {
        if (isMulti) Right(MultiValue(prop.getValues.toList map getValue _))
        else Left("Non multi-value field '%s' can't be set from multi-value property.".format(name))
      }
      else {
        if (isSimple) Right(getValue(prop.getValue))
        else if (isOption) Right(OptionValue(Some(getValue(prop.getValue))))
        else if (isMulti) Left("Multi-value field '%s' can't be set from single-value property.".format(name))
        else Left("Unsupported type %s".format(t))
      }
    }
    else {
      if (isOption) Right(OptionValue(None))
      else if (isMulti) Right(MultiValue(List.empty[SimpleValue[_]]))
      else Left("Simple-value field '%s' can't be set from null-value property.".format(name))
    }
  }

}