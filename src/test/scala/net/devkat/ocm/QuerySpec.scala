package net.devkat.ocm

import com.typesafe.scalalogging.slf4j.Logging

class QuerySpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Query Spec".title
  
  
  "Scala OCM" should {
    
    "Create an object" in transaction {
      val bar = new Bar
      bar.name = "Bar"
      insert(bar, root / "bar")
      1 mustEqual 1
    }
    
    "Lookup an object" in transaction {
      val bar = lookup[Bar](root / "bar")
      bar.size mustEqual 1
    }
    
    "Clean up" in {
      transaction { lookup[Bar](root / "bar") foreach remove _ }
      transaction { lookup[Bar](root / "bar").size mustEqual 0 }
    }
    
  }
}