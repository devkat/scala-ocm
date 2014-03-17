package net.devkat.ocm

import net.devkat.ocm.macros._
import scala.reflect.runtime.universe._
import java.util.Calendar

object MockOcm {
  
  val props = scala.collection.mutable.Map.empty[String, Any]
  
  def readSingleProperty[T](obj: Any, name: String): T =
    props(name).asInstanceOf[T]
  
  def readMultipleProperty[T](obj: Any, name: String): Iterable[T] =
    props(name).asInstanceOf[Iterable[T]]
  
  def readOptionalProperty[T](obj: Any, name: String): Option[T] =
    props(name).asInstanceOf[Option[T]]
  
  def writeSingleProperty[T](obj: Any, name: String, value: T) {
    props.put(name, value)
  }
  
  def writeMultipleProperty[T](obj: Any, name: String, value: Iterable[T]) {
    props.put(name, value)
  }
  
  def writeOptionalProperty[T](obj: Any, name: String, value: Option[T]) {
    props.put(name, value)
  }
}

@node
class Entity {
  import MockOcm._
  
  var binaryP: Array[Byte] = _
  var booleanP: Boolean = _
  var decimalP: BigDecimal = _
  var dateP: Calendar = _
  var doubleP: Double = _
  var longP: Long = _
  var stringP: String = _
  
  var optionalBinaryP: Option[Array[Byte]] = _
  var optionalBooleanP: Option[Boolean] = _
  var optionalDecimalP: Option[BigDecimal] = _
  var optionalDateP: Option[Calendar] = _
  var optionalDoubleP: Option[Double] = _
  var optionalLongP: Option[Long] = _
  var optionalStringP: Option[String] = _
  
  var multiBinaryP: Iterable[Array[Byte]] = _
  var multiBooleanP: Iterable[Boolean] = _
  var multiDecimalP: Iterable[BigDecimal] = _
  var multiDateP: Iterable[Calendar] = _
  var multiDoubleP: Iterable[Double] = _
  var multiLongP: Iterable[Long] = _
  var multiStringP: Iterable[String] = _
  
}
