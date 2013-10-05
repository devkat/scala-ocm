package net.devkat.ocm

import DirectTypeOcm._
import Path.root
import java.util.Calendar
import net.devkat.ocm.DirectTypeOcm._
import net.devkat.ocm.Path.root
import net.devkat.ocm.annotation.JcrProperty
import net.devkat.ocm.nodetype.Folder
import net.devkat.ocm.macros._

@node
class Foo {
  @property var byteArrayProp: Array[Byte] = _
  @property var bigDecimalProp: BigDecimal = _
  @property var booleanProp: Boolean = _
  @property var calendarProp: Calendar = _
  @property var doubleProp: Double = _
  @property var longProp: Long = _
  @property var stringProp: String = _

  @property var byteArrayOptionProp: Option[Array[Byte]] = _
  @property var bigDecimalOptionProp: Option[BigDecimal] = _
  @property var booleanOptionProp: Option[Boolean] = _
  @property var calendarOptionProp: Option[Calendar] = _
  @property var doubleOptionProp: Option[Double] = _
  @property var longOptionProp: Option[Long] = _
  @property var stringOptionProp: Option[String] = _

}

class Bar {
  @property var name: String = _
}
