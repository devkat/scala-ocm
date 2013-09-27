package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging

class QuerySpec extends ScalaOcmSpec with Logging {

  import DirectTypeOcm._
  import Path._

  "Scala OCM Query Spec".title
  
  
  "Scala OCM" should {
    
    "Create an object" in transaction {
      create[Foo](root / "foo")
      1 mustEqual 1
    }
    
    "Lookup an object" in transaction {
      val foo = lookup[Foo](root / "foo")
      foo.size mustEqual 1
    }
    
    "Clean up" in {
      transaction { lookup[Foo](root / "foo") foreach remove _ }
      transaction { lookup[Foo](root / "foo").size mustEqual 0 }
    }
    
  }
}