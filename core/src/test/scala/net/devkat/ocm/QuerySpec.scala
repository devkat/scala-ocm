package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

class QuerySpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Query Spec".title

  "Scala OCM" should {

    "Lookup an object" in {
      transaction {
        val bar = create[Bar](root / "bar")
        bar.name = "Bar"
      }
      transaction {
        val bar = lookup[Bar](root / "bar")
        bar.size mustEqual 1
      }
    }

  }
}