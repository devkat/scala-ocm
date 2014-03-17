package net.devkat.ocm

import scala.reflect.runtime.universe._
import java.util.Calendar
import java.io.ByteArrayInputStream
import javax.jcr.{ Node, Session, Value, ValueFactory }
import org.apache.commons.io.IOUtils

/*
sealed trait JcrType[T] { def jcrPropertyType: Int }

abstract class AbstractJcrType[T](pt: Int) extends JcrType[T] {
  def jcrPropertyType = pt
}

object JcrType {
  case object binary extends AbstractJcrType[Array[Byte]](javax.jcr.PropertyType.BINARY)
  case object boolean extends AbstractJcrType[Boolean](javax.jcr.PropertyType.BOOLEAN)
  case object date extends AbstractJcrType[Calendar](javax.jcr.PropertyType.DATE)
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
*/


/*
object ValueContainer {
  case class simple[T](t: T) extends ValueContainer[T]
  case class optional[T](t: Option[T]) extends ValueContainer[T]
  case class multi[T](t: Iterable[T]) extends ValueContainer[T]
}
*/


sealed trait Cardinality {
}

object Cardinality {
  case object Simple extends Cardinality
  case object Optional extends Cardinality
  case object Multi extends Cardinality
}

sealed trait PropertyType {
  def cardinality: Cardinality
  def jcrPropertyType: Int
}

class BasePropertyType(jcrPropType: Int) {
  def jcrPropertyType = jcrPropType
}

case class CompletePropertyType(t: BasePropertyType, card: Cardinality)
    extends PropertyType {
  def cardinality = card
  def jcrPropertyType = t.jcrPropertyType
}

object PropertyType {
  import Cardinality._
  def simple[T](t: BasePropertyType) = CompletePropertyType(t, Simple)
  def optional[T](t: BasePropertyType) = CompletePropertyType(t, Optional)
  def multi[T](t: BasePropertyType) = CompletePropertyType(t, Multi)

  case object binary extends BasePropertyType(javax.jcr.PropertyType.BINARY)
  case object boolean extends BasePropertyType(javax.jcr.PropertyType.BOOLEAN)
  case object date extends BasePropertyType(javax.jcr.PropertyType.DECIMAL)
  case object decimal extends BasePropertyType(javax.jcr.PropertyType.DECIMAL)
  case object double extends BasePropertyType(javax.jcr.PropertyType.DOUBLE)
  case object long extends BasePropertyType(javax.jcr.PropertyType.LONG)
  case object name extends BasePropertyType(javax.jcr.PropertyType.NAME)
  case object path extends BasePropertyType(javax.jcr.PropertyType.PATH)
  case object reference extends BasePropertyType(javax.jcr.PropertyType.REFERENCE)
  case object string extends BasePropertyType(javax.jcr.PropertyType.STRING)
  case object uri extends BasePropertyType(javax.jcr.PropertyType.URI)
  case object weakReference extends BasePropertyType(javax.jcr.PropertyType.WEAKREFERENCE)
}

sealed trait PropertyAccessor[O[_], M[_]] {
  import javax.jcr.PropertyType._
/*
  def read(t: PropertyType, node: Node, name: String): Any = {
    t.cardinality match {
      case single => readSingle(t, node, name)
      case optional => readOptional(t, node, name)
      case multi => readMultiple(t, node, name)
    }
  }
  
  def write(t: PropertyType, node: Node, name: String, v: Any)(implicit jcrSession: Session) {
    t.cardinality match {
      case single => writeSingle(t, node, name, v)
      case optional => writeOptional(t, node, name, v.asInstanceOf[Option[_]])
      case multi => writeMultiple(t, node, name, v.asInstanceOf[List[_]])
    }
  }
*/
  def readSingle[T](t: PropertyType, node: Node, name: String): T
  def readOptional[T](t: PropertyType, node: Node, name: String): O[T]
  def readMultiple[T](t: PropertyType, node: Node, name: String): M[T]
  
  def writeSingle[T](t: PropertyType, node: Node, name: String, v: T)(implicit jcrSession: Session): Unit
  def writeOptional[T](t: PropertyType, node: Node, name: String, v: O[T])(implicit jcrSession: Session): Unit
  def writeMultiple[T](t: PropertyType, node: Node, name: String, v: M[T])(implicit jcrSession: Session): Unit
}

sealed trait ValueExtractor

object ValueExtractor {
  case object missing extends ValueExtractor
  case class single(v: Value) extends ValueExtractor
  case class multiple(v: Iterable[Value]) extends ValueExtractor

  def apply(node: Node, name: String): ValueExtractor = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) multiple(p.getValues)
      else single(p.getValue)
    } else missing
  }
}

object DefaultPropertyAccessor extends PropertyAccessor[Option, List] {
  import PropertyType._
  import javax.jcr.PropertyType._
  import ValueExtractor._

  protected def typeName(jcrPropertyType: Int) =
    javax.jcr.PropertyType.nameFromValue(jcrPropertyType)

  def checkType(t: PropertyType, v: Value) {
    if (v.getType != t.jcrPropertyType) {
      throw new OcmException(s"Found property type ${typeName(v.getType)}, required type ${typeName(t.jcrPropertyType)}.")
    }
  }
  
  def readValue[T](t: PropertyType)(v: Value): T = {
    checkType(t, v)
    (v.getType match {
      case BINARY => IOUtils.toByteArray(v.getBinary.getStream)
      case BOOLEAN => v.getBoolean
      case DATE => v.getDate
      case DECIMAL => v.getDecimal
      case DOUBLE => v.getDouble
      case LONG => v.getLong
      case NAME => v.getString
      case PATH => v.getString
      case REFERENCE => v.getString
      case STRING => v.getString
      case URI => v.getString
      case WEAKREFERENCE => v.getString
    }).asInstanceOf[T]
  }
  
  def writeValue[T](t: PropertyType)(value: T)(implicit jcrSession: Session): Value = {
    lazy val factory = jcrSession.getValueFactory
    t.jcrPropertyType match {
      case BINARY => factory.createValue(factory.createBinary(new ByteArrayInputStream(value.asInstanceOf[Array[Byte]])))
      case BOOLEAN => factory.createValue(value.asInstanceOf[Boolean])
      case DATE => factory.createValue(value.asInstanceOf[Calendar])
      case DECIMAL => factory.createValue(new java.math.BigDecimal(value.asInstanceOf[BigDecimal].toDouble))
      case DOUBLE => factory.createValue(value.asInstanceOf[Double])
      case LONG => factory.createValue(value.asInstanceOf[Long])
      case t => factory.createValue(value.asInstanceOf[String], t)
    }
  }

  def readSingle[T](t: PropertyType, node: Node, name: String): T = {
    ValueExtractor(node, name) match {
      case single(v) => readValue(t)(v)
      case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
      case missing => throw new OcmException(s"Property ${name} missing.")
    }
  }

  def writeSingle[T](t: PropertyType, node: Node, name: String, v: T)(implicit jcrSession: Session) {
    node.setProperty(name, writeValue(t)(v))
  }

  def readOptional[T](t: PropertyType, node: Node, name: String): Option[T] = {
    ValueExtractor(node, name) match {
      case single(v) => Some(readValue(t)(v))
      case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
      case missing => None.asInstanceOf[Option[T]]
    }
  }

  def writeOptional[T](t: PropertyType, node: Node, name: String, v: Option[T])(implicit jcrSession: Session) {
    v match {
      case Some(w) => node.setProperty(name, writeValue(t)(w))
      case None => if (node.hasProperty(name)) node.getProperty(name).remove()
    }
  }

  def readMultiple[T](t: PropertyType, node: Node, name: String): List[T] = {
    ValueExtractor(node, name) match {
      case single(_) => throw new OcmException(s"Property ${name} is not multiple.")
      case multiple(v) => v.map(readValue(t) _).toList
      case missing => List.empty[T]
    }
  }

  def writeMultiple[T](t: PropertyType, node: Node, name: String, v: List[T])(implicit jcrSession: Session) {
    node.setProperty(name, v.map(writeValue(t) _).toArray)
  }

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