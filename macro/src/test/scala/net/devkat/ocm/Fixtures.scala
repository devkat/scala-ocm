package net.devkat.ocm

import net.devkat.ocm.macros._
import scala.reflect.runtime.universe._
import java.util.Calendar

object MockOcm {
  def readProperty[T : Manifest](obj: Any, name: String, t: PropertyType[T]) = {
    val t = typeOf[T]
    if (t =:= typeOf[String]) "String property value"
    else if (t =:= typeOf[Long]) 3L
  }
  def writeProperty[T](obj: Any, name: String, t: PropertyType[T], value: T) {
    println("Writing property %s (%s)".format(value, value.getClass.getName))
  }
}

@node
class Node {
  import MockOcm._
  
  var binaryP: Array[Byte] = _
  var decimalP: BigDecimal = _
  var dateP: Calendar = _
  var doubleP: Double = _
  var longP: Long = _
  var stringP: String = _
  
  var optionalBinaryP: Option[Array[Byte]] = _
  var optionalDecimalP: Option[BigDecimal] = _
  var optionalDateP: Option[Calendar] = _
  var optionalDoubleP: Option[Double] = _
  var optionalLongP: Option[Long] = _
  var optionalStringP: Option[String] = _
  
  var multiBinaryP: Iterable[Array[Byte]] = _
  var multiDecimalP: Iterable[BigDecimal] = _
  var multiDateP: Iterable[Calendar] = _
  var multiDoubleP: Iterable[Double] = _
  var multiLongP: Iterable[Long] = _
  var multiStringP: Iterable[String] = _
  
}
