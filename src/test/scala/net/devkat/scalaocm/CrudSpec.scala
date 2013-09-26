package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging

class CrudSpec extends ScalaOcmSpec with Logging {
  
  import DirectTypeOcm._
  import Path._

  "Scala OCM CRUD Spec".title

  "Scala OCM" should {
    "Create an object" in transaction {
      val company = create[Company](root / "bec")
      company.name = "BeCompany GmbH"
      update(company)
      company.name mustEqual "BeCompany GmbH"
    }
    
    "Retrieve an object" in transaction {
      val company = lookup[Company](root / "bec").head
      company.name mustEqual "BeCompany GmbH"
    }
    
    "Delete objects" in transaction {
      lookup[Company](root / "bec") foreach remove _
      jcrSession.save()
      lookup[Company](root / "bec") mustEqual Seq.empty
    }
    
    "Create test data" in transaction {
      CompanySchema.createTestData
      1 mustEqual 1
    }
    
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
  }
}