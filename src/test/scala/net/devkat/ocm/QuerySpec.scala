package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

class QuerySpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Query Spec".title

  "Scala OCM" should {

    "Lookup an object" in {
      transaction {
        val bar = new Bar
        bar.name = "Bar"
        insert(bar, root / "bar")
      }
      transaction {
        val bar = lookup[Bar](root / "bar")
        bar.size mustEqual 1
      }
    }

  }
}