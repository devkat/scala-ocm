package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

import net.devkat.ocm.annotation.JcrProperty
import java.util.Calendar
import org.specs2.execute.Result
import java.util.GregorianCalendar

class PropertiesSpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._
  import Extensions._

  "Scala OCM Properties Spec".title

  def checkProperty[T](v: T)(f: (Foo, T) => Unit)(g: Foo => T): Result = {
    try {
      transaction {
        val foo = new Foo
        insert(foo, root / "foo")
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

  val path = root / "foo"

  def bigDecimal(d: Double) = BigDecimal(d, new java.math.MathContext(10))

  "Scala OCM" should {

    // Plain

    "Handle simple type properties" in {
      try {
        transaction {
          val foo = new Foo
          foo.byteArrayProp = Array[Byte](1, 2, 3)
          foo.bigDecimalProp = bigDecimal(3.14)
          foo.booleanProp = true
          foo.calendarProp = new GregorianCalendar(2013, 1, 2)
          foo.doubleProp = 3.14
          foo.longProp = 2L
          foo.stringProp = "Foo"
          insert(foo, root / "foo")
        }
        transaction {
          val foo = lookup[Foo](path).head
          val cal = foo.calendarProp
          foo.byteArrayProp mustEqual Array[Byte](1, 2, 3) and
            (bigDecimal(foo.bigDecimalProp.toDouble) mustEqual bigDecimal(3.14)) and
            (foo.booleanProp mustEqual true) and
            (new GregorianCalendar(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
                mustEqual new GregorianCalendar(2013, 1, 2)) and
            (foo.doubleProp mustEqual 3.14) and
            (foo.longProp mustEqual 2L) and
            (foo.stringProp mustEqual "Foo")
        }
      } finally {
        transaction {
          jcrSession.getRootNode().getNodes("foo") foreach { _.remove() }
          //lookup[Foo](path) map remove _
        }
      }
    }
    
    "Complain about null values" in {
      try {
        transaction {
          val foo = new Foo
          foo.stringProp = null
          insert(foo, root / "foo") must throwAn[OcmException]
        }
      } finally {
        transaction {
          jcrSession.getRootNode().getNodes("foo") foreach { _.remove() }
          //lookup[Foo](path) map remove _
        }
      }
    }
    
    
    /*
    "Handle Array[Byte] properties" in checkProperty(Array[Byte](1, 2, 3)) {
      _.byteArrayProp = _
    } { _.byteArrayProp }

    "Handle BigDecimal properties" in checkProperty(BigDecimal(3.14, new java.math.MathContext(10))) {
      _.bigDecimalProp = _
    } { f => BigDecimal(f.bigDecimalProp.toDouble, new java.math.MathContext(10)) }

    "Handle Boolean properties" in checkProperty(true) {
      _.booleanProp = _
    } { _.booleanProp }

    "Handle Calendar properties" in checkProperty(new GregorianCalendar(2013, 1, 2): Calendar) {
      _.calendarProp = _
    } { f =>
      val cal = f.calendarProp
      new GregorianCalendar(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
    }

    "Handle Double properties" in checkProperty(3.14) {
      _.doubleProp = _
    } { _.doubleProp }

    "Handle Long properties" in checkProperty(2L) {
      _.longProp = _
    } { _.longProp }

    "Handle String properties" in checkProperty("Foo") {
      _.stringProp = _
    } { _.stringProp }

    // Option

    "Handle Some Option[Array[Byte]] properties" in checkProperty(Some(Array[Byte](1, 2, 3, 4)): Option[Array[Byte]]) {
      _.byteArrayOptionProp = _
    } { _.byteArrayOptionProp }

    "Handle None Option[Array[Byte]] properties" in checkProperty(None: Option[Array[Byte]]) {
      _.byteArrayOptionProp = _
    } { _.byteArrayOptionProp }

    "Handle Some Option[BigDecimal] properties" in checkProperty(Some(BigDecimal(4.14, new java.math.MathContext(10))): Option[BigDecimal]) {
      _.bigDecimalOptionProp = _
    } { f => Some(BigDecimal(f.bigDecimalOptionProp.get.toDouble, new java.math.MathContext(10))) }

    "Handle None Option[BigDecimal] properties" in checkProperty(None: Option[BigDecimal]) {
      _.bigDecimalOptionProp = _
    } { _.bigDecimalOptionProp }

    "Handle Some Option[Boolean] properties" in checkProperty(Some(true): Option[Boolean]) {
      _.booleanOptionProp = _
    } { _.booleanOptionProp }

    "Handle None Option[Boolean] properties" in checkProperty(None: Option[Boolean]) {
      _.booleanOptionProp = _
    } { _.booleanOptionProp }

    "Handle Some Option[Calendar] properties" in checkProperty(Some(new GregorianCalendar(2013, 1, 2)): Option[Calendar]) {
      _.calendarOptionProp = _
    } { f =>
      val cal = f.calendarOptionProp.get
      Some(new GregorianCalendar(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH)))
    }

    "Handle None Option[Calendar] properties" in checkProperty(None: Option[Calendar]) {
      _.calendarOptionProp = _
    } { _.calendarOptionProp }

    "Handle Some Option[Double] properties" in checkProperty(Some(3.14): Option[Double]) {
      _.doubleOptionProp = _
    } { _.doubleOptionProp }

    "Handle None Option[Double] properties" in checkProperty(None: Option[Double]) {
      _.doubleOptionProp = _
    } { _.doubleOptionProp }

    "Handle Some Option[Long] properties" in checkProperty(Some(2L): Option[Long]) {
      _.longOptionProp = _
    } { _.longOptionProp }

    "Handle None Option[Long] properties" in checkProperty(None: Option[Long]) {
      _.longOptionProp = _
    } { _.longOptionProp }

    "Handle Some Option[String] properties" in checkProperty(Some("Foo"): Option[String]) {
      _.stringOptionProp = _
    } { _.stringOptionProp }

    "Handle None Option[String] properties" in checkProperty(None: Option[String]) {
      _.stringOptionProp = _
    } { _.stringOptionProp }
*/
  }

}