package net.devkat.ocm

import org.specs2.mutable.Specification
import scala.reflect.runtime.universe._

class MacroSpec extends Specification {
  
  "Macro" should {
    "Set a property" in {
      val foo = new AnnotatedFoo
      foo.name = "Hello"
      foo.name mustEqual "Hello"
    }
  }

}