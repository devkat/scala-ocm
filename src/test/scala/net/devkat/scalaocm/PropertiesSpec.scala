package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging
import net.devkat.scalaocm.annotation.JcrProperty
import java.util.Calendar
import org.specs2.execute.Result

class PropertiesSpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Properties Spec".title

  def checkProperty(f: Foo => Unit)(g: Foo => Result): Result = {
    val path = root / "foo"
    try {
      transaction {
        val foo = create[Foo](root / "foo")
        f(foo)
        update(foo)
      }
      transaction {
        g(lookup[Foo](path).head)
      }
    } finally {
      transaction {
        lookup[Foo](path) map remove _
      }
    }
  }

  "Scala OCM" should {

    "Handle Array[Byte] properties" in checkProperty {
      f => f.byteArrayProp = Array[Byte](1, 2, 3)
    } {
      f => f.byteArrayProp mustEqual Array[Byte](1, 2, 3)
    }

  }

}