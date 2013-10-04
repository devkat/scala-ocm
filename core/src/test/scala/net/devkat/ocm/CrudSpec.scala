package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

import net.devkat.ocm.annotation.JcrProperty

class CrudSpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM CRUD Spec".title

  "Scala OCM" should {

    "Create an object" in transaction {
      val bar = new Bar
      bar.name = "Bar"
      insert(bar, root / "bar")
      1 mustEqual 1
    }

    "Update an object" in {
      transaction {
        val bar = new Bar
        bar.name = "Bar"
        insert(bar, root / "bar")
      }
      transaction {
        val bar = lookup[Bar](root / "bar").head
        bar.name = "Bar changed"
        update(bar)
      }
      transaction {
        val bar = lookup[Bar](root / "bar").head
        bar.name mustEqual "Bar changed"
      }
    }

    "Delete objects" in {
      transaction {
        lookup[Bar](root / "bar") foreach remove _
      }
      transaction {
        lookup[Bar](root / "bar") mustEqual Seq.empty
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