package net.devkat.ocm

import java.io.ByteArrayInputStream
import java.util.Calendar
import org.apache.commons.io.IOUtils
import javax.jcr.Value
import javax.jcr.Session
import javax.jcr.Property
import javax.jcr.PropertyType
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object ValueConversions {

  import Extensions._
  import Reflection._

  def getPropertyType(p: Property): Type = p.getType match {
    case PropertyType.BINARY => typeOf[BinaryValue]
    case PropertyType.BOOLEAN => typeOf[BooleanValue]
    case PropertyType.DATE => typeOf[DateValue]
    case PropertyType.DECIMAL => typeOf[DecimalValue]
    case PropertyType.DOUBLE => typeOf[BooleanValue]
    case PropertyType.LONG => typeOf[LongValue]
    case PropertyType.NAME => typeOf[NameValue]
    case PropertyType.PATH => typeOf[PathValue]
    case PropertyType.REFERENCE => typeOf[ReferenceValue]
    case PropertyType.STRING => typeOf[StringValue]
    case PropertyType.UNDEFINED => typeOf[UndefinedValue]
    case PropertyType.URI => typeOf[UriValue]
    case PropertyType.WEAKREFERENCE => typeOf[WeakReferenceValue]
    case t => throw new OcmException("Unknown property type " + PropertyType.nameFromValue(t))
  }
  
  def getValue(p: Value): SimpleValue[_] = p.getType match {
    case PropertyType.BINARY => BinaryValue(IOUtils.toByteArray(p.getBinary.getStream))
    case PropertyType.BOOLEAN => BooleanValue(p.getBoolean)
    case PropertyType.DATE => DateValue(p.getDate)
    case PropertyType.DECIMAL => DecimalValue(new BigDecimal(p.getDecimal))
    case PropertyType.DOUBLE => DoubleValue(p.getDouble)
    case PropertyType.LONG => LongValue(p.getLong)
    case PropertyType.NAME => NameValue(p.getString)
    case PropertyType.PATH => PathValue(Path.parse(p.getString))
    case PropertyType.REFERENCE => ReferenceValue(p.getString)
    case PropertyType.STRING => StringValue(p.getString)
    case PropertyType.UNDEFINED => UndefinedValue()
    case PropertyType.URI => UriValue(p.getString)
    case PropertyType.WEAKREFERENCE => WeakReferenceValue(p.getString)
    case t => throw new OcmException("Unknown property type " + PropertyType.nameFromValue(t))
  }

  def any2valueOption(v: Any)(implicit jcrSession: Session): Option[Value] = {
    v match {
      case o: Option[_] => o map any2value _
      case _ => Some(any2value(v))
    }
  }

  def any2value(v: Any)(implicit jcrSession: Session): Value = {
    val factory = jcrSession.getValueFactory
    v match {
      case a: Array[Byte] => factory.createValue(factory.createBinary(new ByteArrayInputStream(a)))
      case i: BigDecimal => factory.createValue(new java.math.BigDecimal(i.toDouble))
      case b: Boolean => factory.createValue(b)
      case c: Calendar => factory.createValue(c)
      case d: Double => factory.createValue(d)
      case i: Int => factory.createValue(i)
      case l: Long => factory.createValue(l)
      case s: String => factory.createValue(s, PropertyType.STRING)
      case v => throw new OcmException("Unsupported property value " + v)
    }
  }

  def list2values[T <: Any](l: List[T])(implicit jcrSession: Session): Array[Value] =
    l map any2value _ toArray

}