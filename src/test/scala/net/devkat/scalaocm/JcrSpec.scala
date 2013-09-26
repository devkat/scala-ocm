package net.devkat.scalaocm

import com.typesafe.scalalogging.slf4j.Logging

import Extensions.toIterator

class JcrSpec extends ScalaOcmSpec with Logging {

  import Extensions._
  import DirectTypeOcm._

  "Plain JCR Spec".title

  "OCM with plain JCR" should {

    def root = jcrSession.getRootNode
  
    "Create nodes" in transaction {
      val hello = root.addNode("hello")
      val world = hello.addNode("world")
      world.setProperty("message", "Hello, World!")
      world.getProperty("message").getString mustEqual "Hello, World!"
    }

    "Retrieve nodes" in transaction {
      val node = root.getNode("hello/world")
      node.getPath() mustEqual "/hello/world"
      node.getProperty("message").getString mustEqual "Hello, World!"
    }

    "Remove nodes" in transaction {
      root.getNodes("hello") foreach {_.remove()}
      jcrSession.save()
      root.hasNode("hello/world") mustEqual false
    }
  }
  
}