package net.devkat.ocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import java.util.Calendar
/*
sealed trait Val[T]
case object BinaryVal extends Val[Array[Byte]]

sealed class PropertyValue[T](v: T) {
  def value = v
}

abstract sealed class SimpleValue[T](v: T) extends PropertyValue(v)

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

//case class OptionValue[T, U <: SimpleValue[T]](u: Option[U]) extends PropertyValue[Option[T]](u.map(_.value))
case class OptionValue[T](u: Option[SimpleValue[T]]) extends PropertyValue[Option[T]](u.map(_.value))

//case class MultiValue[T, U <: SimpleValue[T]](v: List[U]) extends PropertyValue[List[T]](v.map(_.value))
case class MultiValue[T](v: List[SimpleValue[T]]) extends PropertyValue[List[T]](v.map(_.value))
*/
/*
abstract class OptionValue[T](v: Option[T]) extends PropertyValue[Option[T]](v)

case class BinaryOptionValue(v: Option[Array[Byte]]) extends OptionValue(v)
case class DecimalOptionValue(v: Option[BigDecimal]) extends OptionValue(v)
case class BooleanOptionValue(v: Option[Boolean]) extends OptionValue(v)
case class DateOptionValue(v: Option[Calendar]) extends OptionValue(v)
case class DoubleOptionValue(v: Option[Double]) extends OptionValue(v)
case class LongOptionValue(v: Option[Long]) extends OptionValue(v)
case class NameOptionValue(v: Option[String]) extends OptionValue(v)
case class PathOptionValue(v: Option[Path]) extends OptionValue(v)
case class ReferenceOptionValue(v: Option[String]) extends OptionValue(v)
case class StringOptionValue(v: Option[String]) extends OptionValue(v)
case class UriOptionValue(v: Option[String]) extends OptionValue(v)
case class WeakReferenceOptionValue(v: Option[String]) extends OptionValue(v)

case class MultiValue[T](v: List[T]) extends PropertyValue(v)

case class BinaryMultiValue(v: List[Array[Byte]]) extends MultiValue(v)
case class DecimalMultiValue(v: List[BigDecimal]) extends MultiValue(v)
case class BooleanMultiValue(v: List[Boolean]) extends MultiValue(v)
case class DateMultiValue(v: List[Calendar]) extends MultiValue(v)
case class DoubleMultiValue(v: List[Double]) extends MultiValue(v)
case class LongMultiValue(v: List[Long]) extends MultiValue(v)
case class NameMultiValue(v: List[String]) extends MultiValue(v)
case class PathMultiValue(v: List[Path]) extends MultiValue(v)
case class ReferenceMultiValue(v: List[String]) extends MultiValue(v)
case class StringMultiValue(v: List[String]) extends MultiValue(v)
case class UriMultiValue(v: List[String]) extends MultiValue(v)
case class WeakReferenceMultiValue(v: List[String]) extends MultiValue(v)
*/
/*
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
        } else if (baseType <:< typeOf[List[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[MultiValue[BinaryValue]])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[MultiValue[DecimalValue]])
              else if (t =:= typeOf[Boolean]) Some(typeOf[MultiValue[BooleanValue]])
              else if (t =:= typeOf[Calendar]) Some(typeOf[MultiValue[DateValue]])
              else if (t =:= typeOf[Double]) Some(typeOf[MultiValue[DoubleValue]])
              else if (t =:= typeOf[Long]) Some(typeOf[MultiValue[LongValue]])
              else if (t =:= typeOf[String]) Some(typeOf[MultiValue[StringValue]])
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
  
  def getPropertyType(baseType: Type): Option[Type] = {
    getSimplePropertyType(baseType) match {
      case t @ Some(_) => t
      case None => {
        if (baseType <:< typeOf[Option[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[OptionValue[Array[Byte], BinaryValue]])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[OptionValue[BigDecimal, DecimalValue]])
              else if (t =:= typeOf[Boolean]) Some(typeOf[OptionValue[Boolean, BooleanValue]])
              else if (t =:= typeOf[Calendar]) Some(typeOf[OptionValue[Calendar, DateValue]])
              else if (t =:= typeOf[Double]) Some(typeOf[OptionValue[Double, DoubleValue]])
              else if (t =:= typeOf[Long]) Some(typeOf[OptionValue[Long, LongValue]])
              else if (t =:= typeOf[String]) Some(typeOf[OptionValue[String, StringValue]])
              else None
            }
            case None => None
          }
        } else if (baseType <:< typeOf[List[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[MultiValue[Array[Byte], BinaryValue]])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[MultiValue[BigDecimal, DecimalValue]])
              else if (t =:= typeOf[Boolean]) Some(typeOf[MultiValue[Boolean, BooleanValue]])
              else if (t =:= typeOf[Calendar]) Some(typeOf[MultiValue[Calendar, DateValue]])
              else if (t =:= typeOf[Double]) Some(typeOf[MultiValue[Double, DoubleValue]])
              else if (t =:= typeOf[Long]) Some(typeOf[MultiValue[Long, LongValue]])
              else if (t =:= typeOf[String]) Some(typeOf[MultiValue[String, StringValue]])
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
  
  def getPropertyType(baseType: Type): Option[Type] = {
    getSimplePropertyType(baseType) match {
      case t @ Some(_) => t
      case None => {
        if (baseType <:< typeOf[Option[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[BinaryOptionValue])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[DecimalOptionValue])
              else if (t =:= typeOf[Boolean]) Some(typeOf[BooleanOptionValue])
              else if (t =:= typeOf[Calendar]) Some(typeOf[DateOptionValue])
              else if (t =:= typeOf[Double]) Some(typeOf[DoubleOptionValue])
              else if (t =:= typeOf[Long]) Some(typeOf[LongOptionValue])
              else if (t =:= typeOf[String]) Some(typeOf[StringOptionValue])
              else None
            }
            case None => None
          }
        } else if (baseType <:< typeOf[List[_]]) {
          baseType find { t => getSimplePropertyType(t).isDefined } match {
            case Some(t) => {
              if (t =:= typeOf[Array[Byte]]) Some(typeOf[BinaryMultiValue])
              else if (t =:= typeOf[BigDecimal]) Some(typeOf[DecimalMultiValue])
              else if (t =:= typeOf[Boolean]) Some(typeOf[BooleanMultiValue])
              else if (t =:= typeOf[Calendar]) Some(typeOf[DateMultiValue])
              else if (t =:= typeOf[Double]) Some(typeOf[DoubleMultiValue])
              else if (t =:= typeOf[Long]) Some(typeOf[LongMultiValue])
              else if (t =:= typeOf[String]) Some(typeOf[StringMultiValue])
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
  */
  /*
    baseType match {
    case typeOf[Array[Byte]] => typeOf[BinaryValue]
  }

}
   */