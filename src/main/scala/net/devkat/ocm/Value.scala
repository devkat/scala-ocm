package net.devkat.ocm

import javax.jcr.Value
import scala.reflect.runtime.universe._
import java.util.Calendar
import javax.jcr.Node
import org.apache.commons.io.IOUtils
import javax.jcr.Session
import java.io.ByteArrayInputStream

sealed abstract class PropertyType[T](propertyType: Int) {
  def set(node: Node, name: String, t: T)(implicit jcrSession: Session): Unit
  def get(node: Node, name: String): T
}

//case object BinaryType extends PropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)

object PropertyHelper {

  def value2any(propertyType: Int)(p: Value): Any =
    propertyType match {
      case javax.jcr.PropertyType.BINARY => IOUtils.toByteArray(p.getBinary.getStream)
      case javax.jcr.PropertyType.BOOLEAN => p.getBoolean
      case javax.jcr.PropertyType.DATE => p.getDate
      case javax.jcr.PropertyType.DECIMAL => new BigDecimal(p.getDecimal)
      case javax.jcr.PropertyType.DOUBLE => p.getDouble
      case javax.jcr.PropertyType.LONG => p.getLong
      case javax.jcr.PropertyType.NAME => p.getString
      case javax.jcr.PropertyType.PATH => Path.parse(p.getString)
      case javax.jcr.PropertyType.REFERENCE => p.getString
      case javax.jcr.PropertyType.STRING => p.getString
      case javax.jcr.PropertyType.UNDEFINED => throw new OcmException("Undefined value")
      case javax.jcr.PropertyType.URI => p.getString
      case javax.jcr.PropertyType.WEAKREFERENCE => p.getString
      case t => throw new OcmException("Unknown property type " + javax.jcr.PropertyType.nameFromValue(t))
    }

  def any2value(propertyType: Int)(v: Any)(implicit jcrSession: Session): Value = {
    val factory = jcrSession.getValueFactory
    v match {
      case a: Array[Byte] => factory.createValue(factory.createBinary(new ByteArrayInputStream(a)))
      case i: BigDecimal => factory.createValue(new java.math.BigDecimal(i.toDouble))
      case b: Boolean => factory.createValue(b)
      case c: Calendar => factory.createValue(c)
      case d: Double => factory.createValue(d)
      case i: Int => factory.createValue(i)
      case l: Long => factory.createValue(l)
      case s: String => factory.createValue(s, propertyType)
      case v => throw new OcmException("Unsupported property value " + v)
    }
  }

}

case class SimpleType[T](propertyType: Int) extends PropertyType[T](propertyType) {

  import PropertyHelper._

  def set(node: Node, name: String, t: T)(implicit jcrSession: Session) {
    node.setProperty(name, any2value(propertyType)(t))
  }

  def get(node: Node, name: String): T = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) {
        throw new OcmException("Can't read single-value field '%s' from multi-value property.".format(name))
      } else {
        value2any(propertyType)(p.getValue).asInstanceOf[T]
      }
    } else {
      throw new OcmException("Property %s.%s not set.".format(node.getPath, name))
    }
  }

}

case class OptionType[T](propertyType: Int) extends PropertyType[Option[T]](propertyType) {

  import PropertyHelper._

  def set(node: Node, name: String, t: Option[T])(implicit jcrSession: Session) {
    t match {
      case Some(value) => node.setProperty(name, any2value(propertyType)(value))
      case None => if (node.hasProperty(name)) node.getProperty(name).remove()
    }
  }

  def get(node: Node, name: String): Option[T] = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) {
        throw new OcmException("Can't read single-value field '%s' from multi-value property.".format(name))
      } else {
        Some(value2any(propertyType)(p.getValue).asInstanceOf[T])
      }
    } else {
      None
    }
  }

}

case class MultiType[T](propertyType: Int) extends PropertyType[List[T]](propertyType) {

  import PropertyHelper._

  def set(node: Node, name: String, t: List[T])(implicit jcrSession: Session) {
    node.setProperty(name, t map any2value(propertyType) _ toArray)
  }

  def get(node: Node, name: String): List[T] = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) {
        p.getValues map (v => value2any(propertyType)(v).asInstanceOf[T]) toList
      } else {
        throw new OcmException("Can't read multi-value field '%s' from single-value property.".format(name))
      }
    } else {
      List.empty[T]
    }
  }

}

object TypeMapper {

  def getSimplePropertyType(baseType: Type): Option[SimpleType[_]] = {
    if (baseType =:= typeOf[Array[Byte]]) Some(SimpleType[Array[Byte]](javax.jcr.PropertyType.BINARY))
    else if (baseType =:= typeOf[BigDecimal]) Some(SimpleType[BigDecimal](javax.jcr.PropertyType.DECIMAL))
    else if (baseType =:= typeOf[Boolean]) Some(SimpleType[Boolean](javax.jcr.PropertyType.BOOLEAN))
    else if (baseType =:= typeOf[Calendar]) Some(SimpleType[Calendar](javax.jcr.PropertyType.DATE))
    else if (baseType =:= typeOf[Double]) Some(SimpleType[Double](javax.jcr.PropertyType.DOUBLE))
    else if (baseType =:= typeOf[Long]) Some(SimpleType[Long](javax.jcr.PropertyType.LONG))
    else if (baseType =:= typeOf[String]) Some(SimpleType[String](javax.jcr.PropertyType.STRING))
    else None
  }

  def getOptionType(baseType: Type): Option[OptionType[_]] = {
    if (baseType =:= typeOf[Array[Byte]]) Some(OptionType[Array[Byte]](javax.jcr.PropertyType.BINARY))
    else if (baseType =:= typeOf[BigDecimal]) Some(OptionType[BigDecimal](javax.jcr.PropertyType.DECIMAL))
    else if (baseType =:= typeOf[Boolean]) Some(OptionType[Boolean](javax.jcr.PropertyType.BOOLEAN))
    else if (baseType =:= typeOf[Calendar]) Some(OptionType[Calendar](javax.jcr.PropertyType.DATE))
    else if (baseType =:= typeOf[Double]) Some(OptionType[Double](javax.jcr.PropertyType.DOUBLE))
    else if (baseType =:= typeOf[Long]) Some(OptionType[Long](javax.jcr.PropertyType.LONG))
    else if (baseType =:= typeOf[String]) Some(OptionType[String](javax.jcr.PropertyType.STRING))
    else None
  }

  def getMultiType(baseType: Type): Option[MultiType[_]] = {
    if (baseType =:= typeOf[Array[Byte]]) Some(MultiType[Array[Byte]](javax.jcr.PropertyType.BINARY))
    else if (baseType =:= typeOf[BigDecimal]) Some(MultiType[BigDecimal](javax.jcr.PropertyType.DECIMAL))
    else if (baseType =:= typeOf[Boolean]) Some(MultiType[Boolean](javax.jcr.PropertyType.BOOLEAN))
    else if (baseType =:= typeOf[Calendar]) Some(MultiType[Calendar](javax.jcr.PropertyType.DATE))
    else if (baseType =:= typeOf[Double]) Some(MultiType[Double](javax.jcr.PropertyType.DOUBLE))
    else if (baseType =:= typeOf[Long]) Some(MultiType[Long](javax.jcr.PropertyType.LONG))
    else if (baseType =:= typeOf[String]) Some(MultiType[String](javax.jcr.PropertyType.STRING))
    else None
  }

  def getPropertyType(baseType: Type): Option[PropertyType[_]] = {
    getSimplePropertyType(baseType) match {
      case t @ Some(_) => t
      case None => {
        if (baseType <:< typeOf[Option[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => getOptionType(t)
            case None => None
          }
        } else if (baseType <:< typeOf[List[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => getMultiType(t)
            case None => None
          }
        } else {
          None
        }
      }
    }
  }
}