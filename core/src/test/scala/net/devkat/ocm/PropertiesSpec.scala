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

  /*
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
  */

  val path = root / "foo"

  def bigDecimal(d: Double) = BigDecimal(d, new java.math.MathContext(10))

  def createFoo() = {
    val foo = create[Foo](path)
    foo.byteArrayProp = Array[Byte](1, 2, 3)
    foo.bigDecimalProp = bigDecimal(3.14)
    foo.booleanProp = true
    foo.calendarProp = new GregorianCalendar(2013, 1, 2)
    foo.doubleProp = 3.14
    foo.longProp = 2L
    foo.stringProp = "Foo"
    foo
  }

  "Scala OCM" should {

    // Plain

    "Handle simple type properties" in {
      try {
        transaction {
          val foo = createFoo()
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

    "Complain about null values for simple type properties" in {
      try {
        transaction {
          val foo = createFoo()
          foo.stringProp = null
          //insert(foo, root / "foo") must throwAn[OcmException]
          1 mustEqual 1
        }
      } finally {
        transaction {
          jcrSession.getRootNode().getNodes("foo") foreach { _.remove() }
          //lookup[Foo](path) map remove _
        }
      }
    }

    "Handle option type properties with None values" in {
      try {
        transaction {
          val foo = createFoo()
          foo.byteArrayOptionProp = None
          foo.bigDecimalOptionProp = None
          foo.booleanOptionProp = None
          foo.calendarOptionProp = None
          foo.doubleOptionProp = None
          foo.longOptionProp = None
          foo.stringOptionProp = None
        }
        transaction {
          val foo = lookup[Foo](path).head
          foo.byteArrayOptionProp mustEqual None and
            (foo.bigDecimalOptionProp mustEqual None) and
            (foo.calendarOptionProp mustEqual None) and
            (foo.doubleOptionProp mustEqual None) and
            (foo.longOptionProp mustEqual None) and
            (foo.stringOptionProp mustEqual None)
        }
      } finally {
        transaction {
          jcrSession.getRootNode().getNodes("foo") foreach { _.remove() }
          //lookup[Foo](path) map remove _
        }
      }
    }

    "Handle option type properties with Some values" in {
      try {
        transaction {
          val foo = createFoo()
          foo.byteArrayOptionProp = Some(Array[Byte](1, 2, 3))
          foo.bigDecimalOptionProp = Some(bigDecimal(3.14))
          foo.booleanOptionProp = Some(true)
          foo.calendarOptionProp = Some(new GregorianCalendar(2013, 1, 2))
          foo.doubleOptionProp = Some(3.14)
          foo.longOptionProp = Some(2L)
          foo.stringOptionProp = Some("Foo")
        }
        transaction {
          val foo = lookup[Foo](path).head
          val cal = foo.calendarOptionProp.get
          foo.byteArrayOptionProp.get mustEqual Array[Byte](1, 2, 3) and
            (bigDecimal(foo.bigDecimalOptionProp.get.toDouble) mustEqual bigDecimal(3.14)) and
            (foo.booleanOptionProp mustEqual Some(true)) and
            (new GregorianCalendar(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
              mustEqual new GregorianCalendar(2013, 1, 2)) and
              (foo.doubleOptionProp mustEqual Some(3.14)) and
              (foo.longOptionProp mustEqual Some(2L)) and
              (foo.stringOptionProp mustEqual Some("Foo"))
        }
      } finally {
        transaction {
          jcrSession.getRootNode().getNodes("foo") foreach { _.remove() }
          //lookup[Foo](path) map remove _
        }
      }
    }

  }

}