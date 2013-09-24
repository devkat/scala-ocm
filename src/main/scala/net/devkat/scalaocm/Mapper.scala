package net.devkat.scalaocm

import javax.jcr.Node
import java.util.Calendar
import javax.jcr.Binary
import javax.jcr.Value
import org.apache.commons.io.IOUtils
import javax.jcr.PropertyType
import java.io.ByteArrayInputStream

trait Mapper[T <: JcrNode[T]] {
  self: JcrNode[T] =>

  import Ocm._
  import Extensions._

  def value2any(p: Value) = p.getType match {
    case PropertyType.BINARY => IOUtils.toByteArray(p.getBinary.getStream)
    case PropertyType.BOOLEAN => p.getBoolean
    case PropertyType.DATE => p.getDate
    case PropertyType.DECIMAL => p.getDecimal
    case PropertyType.DOUBLE => p.getDouble
    case PropertyType.LONG => p.getLong
    case PropertyType.NAME => p.getString
    case PropertyType.PATH => p.getString
    case PropertyType.REFERENCE => p.getString
    case PropertyType.STRING => p.getString
    case PropertyType.UNDEFINED => throw new RuntimeException("Undefined property type.")
    case PropertyType.URI => p.getString
    case PropertyType.WEAKREFERENCE => p.getString
    case t => throw new RuntimeException("Unknown property type " + PropertyType.nameFromValue(t))
  }

  implicit def scalaBigDecimal2javaBigDecimal(i: BigDecimal): java.math.BigDecimal =
    new java.math.BigDecimal(i.toDouble)

  implicit def byteArray2binary(a: Array[Byte]): Binary =
    jcrSession.getValueFactory().createBinary(new ByteArrayInputStream(a))

  def setProperty(name: String, v: Any) = withNode { n =>
    v match {
      case null => n.setProperty(name, null.asInstanceOf[String])
      case l: List[_] => n.setProperty(name, toJcrValues(l))
      case v => n.setProperty(name, toJcrValue(v))
    }
  }

  lazy val factory = jcrSession.getValueFactory

  def toJcrValues[T <: Any](l: List[T]): Array[Value] = l map toJcrValue _ toArray

  def toJcrValue(v: Any): Value = v match {
    case a: Array[Byte] => factory.createValue(a)
    case i: BigDecimal => factory.createValue(i)
    case b: Boolean => factory.createValue(b)
    case c: Calendar => factory.createValue(c)
    case d: Double => factory.createValue(d)
    case i: Int => factory.createValue(i)
    case l: Long => factory.createValue(l)
    case s: String => factory.createValue(s, PropertyType.STRING)
    case v => throw new RuntimeException("Unsupported property value " + v)
  }

}