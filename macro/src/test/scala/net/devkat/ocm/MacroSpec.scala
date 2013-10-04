package net.devkat.ocm

import org.specs2.mutable.Specification
import java.util.Calendar

class MacroSpec extends Specification {
  
  "Macro" should {
    "Set a property" in {
      val node = new Node
      (node.stringP mustEqual "String property value") and
      (node.longP mustEqual 3L)
    }
  }

}