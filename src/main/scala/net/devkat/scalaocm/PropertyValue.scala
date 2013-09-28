package net.devkat.scalaocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import java.util.Calendar

sealed class PropertyValue[T](v: T) {
  def value = v
}

abstract class SimpleValue[T](v: T) extends PropertyValue(v)

case class BinaryValue(v: Array[Byte]) extends SimpleValue(v)
case class DecimalValue(v: BigDecimal) extends SimpleValue(v)
case class BooleanValue(v: Boolean) extends SimpleValue(v)
case class DateValue(v: Calendar) extends SimpleValue(v)
case class DoubleValue(v: Double) extends SimpleValue(v)
case class LongValue(v: Long) extends SimpleValue(v)
case class NameValue(v: String) extends SimpleValue(v)
case class PathValue(v: Path) extends SimpleValue(v)
case class ReferenceValue(v: String) extends SimpleValue(v)
case class StringValue(v: String) extends SimpleValue(v)
case class UriValue(v: String) extends SimpleValue(v)
case class WeakReferenceValue(v: String) extends SimpleValue(v)
case class UndefinedValue() extends SimpleValue

case class OptionValue[T <: SimpleValue[_]](v: Option[T]) extends PropertyValue(v)

case class MultiValue[T <: SimpleValue[_]](v: List[T]) extends PropertyValue(v)

object TypeMapper {

  def getSimplePropertyType(baseType: Type): Option[Type] = {
    if (baseType =:= typeOf[Array[Byte]]) Some(typeOf[BinaryValue])
    else if (baseType =:= typeOf[BigDecimal]) Some(typeOf[DecimalValue])
    else if (baseType =:= typeOf[Boolean]) Some(typeOf[BooleanValue])
    else if (baseType =:= typeOf[Calendar]) Some(typeOf[DateValue])
    else if (baseType =:= typeOf[Double]) Some(typeOf[DoubleValue])
    else if (baseType =:= typeOf[Long]) Some(typeOf[LongValue])
    else if (baseType =:= typeOf[String]) Some(typeOf[StringValue])
    else None
  }

  def getPropertyType(baseType: Type): Option[Type] = {
    getSimplePropertyType(baseType) match {
      case t @ Some(_) => t
      case None => {
        if (baseType <:< typeOf[Option[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[OptionValue[BinaryValue]])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[OptionValue[DecimalValue]])
              else if (t =:= typeOf[Boolean]) Some(typeOf[OptionValue[BooleanValue]])
              else if (t =:= typeOf[Calendar]) Some(typeOf[OptionValue[DateValue]])
              else if (t =:= typeOf[Double]) Some(typeOf[OptionValue[DoubleValue]])
              else if (t =:= typeOf[Long]) Some(typeOf[OptionValue[LongValue]])
              else if (t =:= typeOf[String]) Some(typeOf[OptionValue[StringValue]])
              else None
            }
            case None => None
          }
        } else {
          None
        }
      }
    }
  }
  /*
    baseType match {
    case typeOf[Array[Byte]] => typeOf[BinaryValue]
  }
  */

}