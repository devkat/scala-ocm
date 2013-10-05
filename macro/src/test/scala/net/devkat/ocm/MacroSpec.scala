package net.devkat.ocm

import org.specs2.mutable.Specification
import java.util.Calendar
import java.util.GregorianCalendar

class MacroSpec extends Specification {

  "Macro" should {
    "Set and get properties" in {

      val binaryV: Array[Byte] = Array(1, 2, 3)
      val booleanV: Boolean = true
      val decimalV: BigDecimal = BigDecimal(3L)
      val dateV: Calendar = new GregorianCalendar(1977, 19, 4)
      val doubleV: Double = 3.14
      val longV: Long = 7L
      val stringV: String = "Hello World"

      val entity = new Entity
      entity.binaryP = binaryV
      entity.booleanP = booleanV
      entity.decimalP = decimalV
      entity.dateP = dateV
      entity.doubleP = doubleV
      entity.longP = longV
      entity.stringP = stringV

      entity.optionalBinaryP = Some(binaryV)
      entity.optionalBooleanP = Some(booleanV)
      entity.optionalDecimalP = Some(decimalV)
      entity.optionalDateP = Some(dateV)
      entity.optionalDoubleP = Some(doubleV)
      entity.optionalLongP = Some(longV)
      entity.optionalStringP = Some(stringV)

      entity.multiBinaryP = List(binaryV)
      entity.multiBooleanP = List(booleanV)
      entity.multiDecimalP = List(decimalV)
      entity.multiDateP = List(dateV)
      entity.multiDoubleP = List(doubleV)
      entity.multiLongP = List(longV)
      entity.multiStringP = List(stringV)

      (entity.binaryP mustEqual binaryV) and
        (entity.booleanP mustEqual booleanV) and
        (entity.decimalP mustEqual decimalV) and
        (entity.dateP mustEqual dateV) and
        (entity.doubleP mustEqual doubleV) and
        (entity.longP mustEqual longV) and
        (entity.stringP mustEqual stringV) and
        (entity.optionalBinaryP mustEqual Some(binaryV)) and
        (entity.optionalBooleanP mustEqual Some(booleanV)) and
        (entity.optionalDecimalP mustEqual Some(decimalV)) and
        (entity.optionalDateP mustEqual Some(dateV)) and
        (entity.optionalDoubleP mustEqual Some(doubleV)) and
        (entity.optionalLongP mustEqual Some(longV)) and
        (entity.optionalStringP mustEqual Some(stringV)) and
        (entity.multiBinaryP mustEqual List(binaryV)) and
        (entity.multiBooleanP mustEqual List(booleanV)) and
        (entity.multiDecimalP mustEqual List(decimalV)) and
        (entity.multiDateP mustEqual List(dateV)) and
        (entity.multiDoubleP mustEqual List(doubleV)) and
        (entity.multiLongP mustEqual List(longV)) and
        (entity.multiStringP mustEqual List(stringV))
    }
  }

}