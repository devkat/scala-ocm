package net.devkat.scalaocm

abstract class Schema(val classes: Class[_ <: JcrNode[_]]*) {
  
  import Ocm._
  import Extensions._

  def removeAll() {
    jcrSession.getRootNode.childNodes foreach { _.remove() }
  }

}