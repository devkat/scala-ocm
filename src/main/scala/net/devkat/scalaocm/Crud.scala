package net.devkat.scalaocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import javax.jcr.Node
import net.devkat.scalaocm.annotation.JcrProperty

trait Crud {
  
  self: Mapper with SessionHolder =>
  
  import Reflection._

  private val classNameProperty = "class"

  implicit def path(r: AnyRef) = withNode(r) { jcrPath _ }

  def create[T <: AnyRef](path: Path)(implicit m: Manifest[T]): T = {
    val r = newInstance[T]
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    node.setProperty(scalaOcmNamespace.prefixed(classNameProperty), getClass.getName)
    node2obj.value.put(node, r)
    r
  }

  def update(r: AnyRef) {
    save(r)
  }
  
  def remove(r: AnyRef) {
    withNode(r) { _.remove() }
  }
  
}