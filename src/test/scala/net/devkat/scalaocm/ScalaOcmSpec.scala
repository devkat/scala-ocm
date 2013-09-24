package net.devkat.scalaocm

import org.specs2.specification.AroundExample
import org.specs2.mutable.Specification
import org.specs2.execute.Result
import org.specs2.execute.AsResult
import org.apache.jackrabbit.core.TransientRepository
import Ocm._
import Path._
import Extensions.toIterator
import com.typesafe.scalalogging.slf4j.Logging

class JcrRecordSpec extends Specification with AroundExample with Logging {
  
  import Extensions._
  import JcrHelpers._

  "JCR Record Specification".title
  sequential

  protected def around[T: AsResult](t: => T): Result = {
    withRepo(new TransientRepository) {
      AsResult(t)
    }
  }

  "JCR Record" should {
    "Create new nodes" in {
      val root = jcrSession.getRootNode
      
      // Store content 
      val hello = root.addNode("hello")
      val world = hello.addNode("world")
      world.setProperty("message", "Hello, World!")
      jcrSession.save()

      // Retrieve content 
      val node = root.getNode("hello/world")
      node.getPath() mustEqual "/hello/world"
      node.getProperty("message").getString mustEqual "Hello, World!"

      // Remove content 
      root.getNodes("hello") foreach {_.remove()}
      jcrSession.save()
      root.hasNode("hello/world") mustEqual false
    }
    
    "Create an object" in {
      val company = new Company().insertAt(root / "bec")
      company.name = "BeCompany GmbH"
      company.save()
      dump(company.jcrNode.get)
      company.name mustEqual "BeCompany GmbH"
    }
    
    "Retrieve an object" in {
      val company = Company.find(root / "bec").headOption.get
      company.name mustEqual "BeCompany GmbH"
    }
    
    "Delete objects" in {
      Company.find(root / "bec") foreach { _.remove() }
      jcrSession.save()
      Company.find(root / "bec") mustEqual Seq.empty
    }
    
    "Create test data" in {
      CompanySchema.createTestData
      1 mustEqual 1
    }
    
    "Manage references" in {
      val r1 = Room.find(root / "rooms" / "room1").head
      val r2 = Room.find(root / "rooms" / "room2").head
      val rooms = List(r1, r2)
      val employee = Employee.find(root / "first" / "peter_example").head
      employee.rooms mustEqual (rooms map (_.identifier))
    }
    
    "Clean up" in {
      CompanySchema.cleanup
      1 mustEqual 1
    }
  }

}