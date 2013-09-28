package net.devkat.ocm

import DirectTypeOcm._
import Path.root
import java.util.Calendar

import net.devkat.ocm.DirectTypeOcm._
import net.devkat.ocm.Path.root
import net.devkat.ocm.annotation.JcrProperty
import net.devkat.ocm.node.Folder

class Foo {
  @JcrProperty var byteArrayProp: Array[Byte] = _
  @JcrProperty var bigDecimalProp: BigDecimal = _
  @JcrProperty var booleanProp: Boolean = _
  @JcrProperty var calendarProp: Calendar = _
  @JcrProperty var doubleProp: Double = _
  @JcrProperty var longProp: Long = _
  @JcrProperty var stringProp: String = _

  @JcrProperty var byteArrayOptionProp: Option[Array[Byte]] = _
  @JcrProperty var bigDecimalOptionProp: Option[BigDecimal] = _
  @JcrProperty var booleanOptionProp: Option[Boolean] = _
  @JcrProperty var calendarOptionProp: Option[Calendar] = _
  @JcrProperty var doubleOptionProp: Option[Double] = _
  @JcrProperty var longOptionProp: Option[Long] = _
  @JcrProperty var stringOptionProp: Option[String] = _

}

class Bar {
  @JcrProperty var name: String = _
}