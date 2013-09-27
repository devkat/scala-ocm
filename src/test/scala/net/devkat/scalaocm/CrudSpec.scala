package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging
import net.devkat.scalaocm.annotation.JcrProperty

class CrudSpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM CRUD Spec".title

  "Scala OCM" should {

    "Create an object" in transaction {
      create[Foo](root / "foo")
      1 mustEqual 1
    }

    "Update an object" in {
      transaction {
        val foo = lookup[Foo](root / "foo").head
        foo.stringProp = "Foo"
        update(foo)
      }
      transaction {
        val foo = lookup[Foo](root / "foo").head
        foo.stringProp mustEqual "Foo"
      }
    }

    "Delete objects" in {
      transaction {
        lookup[Foo](root / "foo") foreach remove _
      }
      transaction {
        lookup[Foo](root / "foo") mustEqual Seq.empty
      }
    }

    /*
    "Manage references" in transaction {
      val r1 = lookup[Room](root / "rooms" / "room1").head
      val r2 = lookup[Room](root / "rooms" / "room2").head
      val rooms = List(r1, r2)
      val employee = lookup[Employee](root / "first" / "peter_example").head
      employee.rooms mustEqual (rooms map { n => identifier(n) })
    }

    "Clean up" in transaction {
      CompanySchema.cleanup
      1 mustEqual 1
    }
    */
  }
}