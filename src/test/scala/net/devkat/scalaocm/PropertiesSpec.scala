package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging
import net.devkat.scalaocm.annotation.JcrProperty
import java.util.Calendar
import org.specs2.execute.Result
import java.util.GregorianCalendar

class PropertiesSpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Properties Spec".title

  def checkProperty[T](v:T)(f: (Foo, T) => Unit)(g: Foo => T): Result = {
    val path = root / "foo"
    try {
      transaction {
        val foo = create[Foo](root / "foo")
        f(foo, v)
        update(foo)
      }
      transaction {
        g(lookup[Foo](path).head) mustEqual v
      }
    } finally {
      transaction {
        lookup[Foo](path) map remove _
      }
    }
  }

  "Scala OCM" should {

    "Handle Array[Byte] properties" in checkProperty(Array[Byte](1, 2, 3)) {
      _.byteArrayProp = _ } { _.byteArrayProp }

    "Handle BigDecimal properties" in checkProperty(BigDecimal(3.14, new java.math.MathContext(10))) {
      _.bigDecimalProp = _ } { f => BigDecimal(f.bigDecimalProp.toDouble, new java.math.MathContext(10)) }

    "Handle Boolean properties" in checkProperty(true) {
      _.booleanProp = _ } { _.booleanProp }

    "Handle Calendar properties" in checkProperty(new GregorianCalendar(2013, 1, 2):Calendar) {
      _.calendarProp = _ } { f =>
      val cal = f.calendarProp
      new GregorianCalendar(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
    }

    "Handle Double properties" in checkProperty(3.14) {
      _.doubleProp = _ } { _.doubleProp }

    "Handle Long properties" in checkProperty(2L) {
      _.longProp = _ } { _.longProp }

    "Handle String properties" in checkProperty("Foo") {
      _.stringProp = _ } { _.stringProp }

  }

}