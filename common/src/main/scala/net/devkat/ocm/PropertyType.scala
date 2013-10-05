package net.devkat.ocm

import scala.reflect.runtime.universe._
import java.util.Calendar

sealed trait JcrType[T] { def jcrPropertyType: Int }

abstract class AbstractJcrType[T](pt: Int) extends JcrType[T] {
  def jcrPropertyType = pt
}

object JcrType {
  case object binary extends AbstractJcrType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object boolean extends AbstractJcrType[Boolean](javax.jcr.PropertyType.BOOLEAN)
  case object date extends AbstractJcrType[Calendar](javax.jcr.PropertyType.DECIMAL)
  case object decimal extends AbstractJcrType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object double extends AbstractJcrType[Double](javax.jcr.PropertyType.DOUBLE)
  case object long extends AbstractJcrType[Long](javax.jcr.PropertyType.LONG)
  case object name extends AbstractJcrType[String](javax.jcr.PropertyType.NAME)
  case object path extends AbstractJcrType[String](javax.jcr.PropertyType.PATH)
  case object reference extends AbstractJcrType[String](javax.jcr.PropertyType.REFERENCE)
  case object string extends AbstractJcrType[String](javax.jcr.PropertyType.STRING)
  case object uri extends AbstractJcrType[String](javax.jcr.PropertyType.URI)
  case object weakReference extends AbstractJcrType[String](javax.jcr.PropertyType.WEAKREFERENCE)
}

//sealed trait PropertyType[T] { def jcrPropertyType: Int }

abstract class PropertyType[T](pt: Int) {
  def jcrPropertyType = pt
}

object PropertyType {
  case class simple[T](t: JcrType[T]) extends PropertyType[T](t.jcrPropertyType)
  case class optional[T](t: JcrType[T]) extends PropertyType[Option[T]](t.jcrPropertyType)
  case class multi[T](t: JcrType[T]) extends PropertyType[Iterable[T]](t.jcrPropertyType)
}

/*
object PropertyType {
  case object binary extends SimplePropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object boolean extends SimplePropertyType[Boolean](javax.jcr.PropertyType.BOOLEAN)
  case object date extends SimplePropertyType[Calendar](javax.jcr.PropertyType.DECIMAL)
  case object decimal extends SimplePropertyType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object double extends SimplePropertyType[Double](javax.jcr.PropertyType.DOUBLE)
  case object long extends SimplePropertyType[Long](javax.jcr.PropertyType.LONG)
  case object name extends SimplePropertyType[String](javax.jcr.PropertyType.NAME)
  case object path extends SimplePropertyType[String](javax.jcr.PropertyType.PATH)
  case object reference extends SimplePropertyType[String](javax.jcr.PropertyType.REFERENCE)
  case object string extends SimplePropertyType[String](javax.jcr.PropertyType.STRING)
  case object uri extends SimplePropertyType[String](javax.jcr.PropertyType.URI)
  case object weakReference extends SimplePropertyType[String](javax.jcr.PropertyType.WEAKREFERENCE)
  
  case object optionalBinary extends OptionalPropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object optionalBoolean extends OptionalPropertyType[Boolean](javax.jcr.PropertyType.BOOLEAN)
  case object optionalDate extends OptionalPropertyType[Calendar](javax.jcr.PropertyType.DATE)
  case object optionalDecimal extends OptionalPropertyType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object optionalDouble extends OptionalPropertyType[Double](javax.jcr.PropertyType.DOUBLE)
  case object optionalLong extends OptionalPropertyType[Long](javax.jcr.PropertyType.LONG)
  case object optionalName extends OptionalPropertyType[String](javax.jcr.PropertyType.NAME)
  case object optionalPath extends OptionalPropertyType[String](javax.jcr.PropertyType.PATH)
  case object optionalReference extends OptionalPropertyType[String](javax.jcr.PropertyType.REFERENCE)
  case object optionalString extends OptionalPropertyType[String](javax.jcr.PropertyType.STRING)
  case object optionalUri extends OptionalPropertyType[String](javax.jcr.PropertyType.URI)
  case object optionalWeakReference extends OptionalPropertyType[String](javax.jcr.PropertyType.WEAKREFERENCE)
  
  case object multiBinary extends MultiPropertyType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object multiBoolean extends MultiPropertyType[Boolean](javax.jcr.PropertyType.BOOLEAN)
  case object multiDate extends MultiPropertyType[Calendar](javax.jcr.PropertyType.DATE)
  case object multiDecimal extends MultiPropertyType[BigDecimal](javax.jcr.PropertyType.DECIMAL)
  case object multiDouble extends MultiPropertyType[Double](javax.jcr.PropertyType.DOUBLE)
  case object multiLong extends MultiPropertyType[Long](javax.jcr.PropertyType.LONG)
  case object multiName extends MultiPropertyType[String](javax.jcr.PropertyType.NAME)
  case object multiPath extends MultiPropertyType[String](javax.jcr.PropertyType.PATH)
  case object multiReference extends MultiPropertyType[String](javax.jcr.PropertyType.REFERENCE)
  case object multiString extends MultiPropertyType[String](javax.jcr.PropertyType.STRING)
  case object multiUri extends MultiPropertyType[String](javax.jcr.PropertyType.URI)
  case object multiWeakReference extends MultiPropertyType[String](javax.jcr.PropertyType.WEAKREFERENCE)
}
*/