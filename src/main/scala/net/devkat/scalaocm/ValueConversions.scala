package net.devkat.scalaocm

import java.io.ByteArrayInputStream
import java.util.Calendar

import org.apache.commons.io.IOUtils

import javax.jcr.PropertyType
import javax.jcr.Value

object ValueConversions {

  import Ocm._
  import Extensions._

  lazy val factory = jcrSession.getValueFactory

  def value2any(p: Value): Any = p.getType match {
    case PropertyType.BINARY => IOUtils.toByteArray(p.getBinary.getStream)
    case PropertyType.BOOLEAN => p.getBoolean
    case PropertyType.DATE => p.getDate
    case PropertyType.DECIMAL => new BigDecimal(p.getDecimal)
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

  def any2value(v: Any): Value = v match {
    case a: Array[Byte] => factory.createValue(factory.createBinary(new ByteArrayInputStream(a)))
    case i: BigDecimal => factory.createValue(new java.math.BigDecimal(i.toDouble))
    case b: Boolean => factory.createValue(b)
    case c: Calendar => factory.createValue(c)
    case d: Double => factory.createValue(d)
    case i: Int => factory.createValue(i)
    case l: Long => factory.createValue(l)
    case s: String => factory.createValue(s, PropertyType.STRING)
    case v => throw new RuntimeException("Unsupported property value " + v)
  }

  def list2values[T <: Any](l: List[T]): Array[Value] = l map any2value _ toArray

}