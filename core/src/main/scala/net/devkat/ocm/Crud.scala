package net.devkat.ocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import javax.jcr.Node
import net.devkat.ocm.annotation.JcrProperty

trait Crud {
  
  self: Mapper with SessionHolder =>
  
  import Reflection._

  private val classNameProperty = "class"

  implicit def path(r: AnyRef) = withNode(r) { jcrPath _ }

  def insert(r: AnyRef, path: Path) {
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    node.setProperty(scalaOcmNamespace.prefixed(classNameProperty), r.getClass.getName)
    node2obj.value.put(node, r)
    save(r)
  }

  def update(r: AnyRef) {
    save(r)
  }
  
  def remove(r: AnyRef) {
    withNode(r) { _.remove() }
  }
  
}