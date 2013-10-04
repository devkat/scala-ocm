package net.devkat.ocm

import org.specs2.mutable.Specification
import net.devkat.ocm.macros._
/*
@node
class AnnotatedFoo {
  import DirectTypeOcm._
  
  @property(propertyType = PropertyType.string)
  var name: String = "hi"
    
  var unmapped: String = "ho"
    
  @property
  var age: Long = 5L
}

class MacroSpec extends Specification {
  
  "Macro" should {
    "Set a property" in {
      val foo = new AnnotatedFoo
      foo.name = "Hello"
      foo.age = 7L
      
      (foo.name mustEqual "String property value") and
      (foo.age mustEqual 3L)
    }
  }

}
*/