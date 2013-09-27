package net.devkat.scalaocm

import DirectTypeOcm._
import Path.root
import java.util.Calendar
import net.devkat.scalaocm.annotation.JcrProperty
import net.devkat.scalaocm.node.Folder

  class Foo {
    @JcrProperty var byteArrayProp: Array[Byte] = _
    @JcrProperty var bigDecimalProp: BigDecimal = _
    @JcrProperty var booleanProp: Boolean = _
    @JcrProperty var calendarProp: Calendar = _
    @JcrProperty var doubleProp: Double = _
    @JcrProperty var longProp: Long = _
    @JcrProperty var stringProp: String = _
  }
