package net.devkat.ocm

import javax.jcr.Node

trait Query {
  
  self: SessionHolder with Mapper =>

  import Extensions._
  
  protected def findNodes(path: Path, parent: Node = jcrSession.getRootNode): Seq[Node] = {
    path.names match {
      case Nil => Nil
      case step :: Nil => parent.getNodes(step).toSeq
      case head :: tail => {
        val nodes = parent.getNodes(head)
        nodes.flatMap(n => findNodes(Path(tail, false), n)).toSeq
      }
    }
  }

  def lookup[T <: AnyRef](path: Path)(implicit m: Manifest[T]): Seq[T] =
    findNodes(path).map(fromNode[T] _).toSeq

  def parentOf[T <: AnyRef](child: AnyRef)(implicit m: Manifest[T]): Option[T] = withNode(child) { n =>
    val parent = n.getParent
    parent.getPath() match {
      case "" => None
      case p => Some(fromNode(parent))
    }
  }

  def referencingFields(r: AnyRef): Iterator[(AnyRef, String)] = withNode(r) { node =>
    node.getReferences map { property => (fromNode(property.getParent), property.getName) }
  }
  
}