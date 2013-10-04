package net.devkat.ocm

import scala.reflect.runtime.universe._
import java.util.Calendar

sealed trait PropertyType[T] { def jcrPropertyType: Int }

abstract class PropertyTypeBase[T](pt: Int) extends PropertyType[T] {
  def jcrPropertyType = pt
}

abstract class OptionalPropertyType[T](pt: Int) extends PropertyTypeBase[Option[T]](pt)

abstract class MultiPropertyType[T](pt: Int) extends PropertyTypeBase[Iterable[T]](pt)

object PropertyType {
  case object binary extends PropertyTypeBase[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object date extends PropertyTypeBase[Calendar](javax.jcr.PropertyType.DECIMAL)
  case object decimal extends PropertyTypeBase[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object double extends PropertyTypeBase[Double](javax.jcr.PropertyType.DOUBLE)
  case object long extends PropertyTypeBase[Long](javax.jcr.PropertyType.LONG)
  case object string extends PropertyTypeBase[String](javax.jcr.PropertyType.STRING)
  
  case object optionalBinary extends OptionalPropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object optionalDate extends OptionalPropertyType[Calendar](javax.jcr.PropertyType.DATE)
  case object optionalDecimal extends OptionalPropertyType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object optionalDouble extends OptionalPropertyType[Double](javax.jcr.PropertyType.DOUBLE)
  case object optionalLong extends OptionalPropertyType[Long](javax.jcr.PropertyType.LONG)
  case object optionalString extends OptionalPropertyType[String](javax.jcr.PropertyType.STRING)
  
  case object multiBinary extends MultiPropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object multiDate extends MultiPropertyType[Calendar](javax.jcr.PropertyType.DATE)
  case object multiDecimal extends MultiPropertyType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object multiDouble extends MultiPropertyType[Double](javax.jcr.PropertyType.DOUBLE)
  case object multiLong extends MultiPropertyType[Long](javax.jcr.PropertyType.LONG)
  case object multiString extends MultiPropertyType[String](javax.jcr.PropertyType.STRING)
  
  /*
  def defaultPropertyType[T:TypeTag](): PropertyType[T] = {
    val t = typeOf[T]
    val propType =
    if (t =:= typeOf[Array[Byte]]) binary
    else if (t =:= typeOf[Long]) long
    else if (t =:= typeOf[String]) string
    else throw new RuntimeException(s"Unsupported property type ${t}")
    propType.asInstanceOf[PropertyType[T]]
  }
  */
}

