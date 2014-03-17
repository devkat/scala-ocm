package net.devkat.ocm

import scala.util.DynamicVariable
import javax.jcr.Node
import javax.jcr.Session
import javax.jcr.Repository
import javax.jcr.SimpleCredentials
import com.typesafe.scalalogging.slf4j.Logging
import net.devkat.ocm.OcmException

trait SessionHolder extends Logging {

  protected val repository = new DynamicVariable[Repository](null)
  protected val currentSession = new DynamicVariable[Session](null)
  protected val node2obj = new DynamicVariable[scala.collection.mutable.Map[Node, AnyRef]](null)

  implicit def jcrSession = currentSession.value match {
    case null => throw new OcmException("Not logged in to repository.")
    case s => s
  }

  val scalaOcmNamespace = Namespace("socm", "http://scalaocm.org/ns")

  def withRepo[T](repo: Repository)(f: => T): T = {
    repository.withValue(repo) {
      inSession { session =>
        val nsReg = jcrSession.getWorkspace.getNamespaceRegistry
        if (!nsReg.getURIs.contains(scalaOcmNamespace.uri))
          nsReg.registerNamespace(scalaOcmNamespace.prefix, scalaOcmNamespace.uri)
      }
      f
    }
  }

  def inSession[T](f: Session => T): T = {
    val repo = repository.value
    val session = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    try {
      logger.info("Logged in as {} to a {} repository.", session.getUserID,
        repo.getDescriptor(Repository.REP_NAME_DESC))
      currentSession.withValue(session) {
        f(session)
      }
    } finally {
      session.logout()
    }
  }

  def transaction[T](f: => T): T = {
    inSession { session =>
      val ret = node2obj.withValue(scala.collection.mutable.Map.empty[Node, AnyRef]) {
        f
      }
      session.save()
      ret
    }
  }

  protected[ocm] def withNode[T <: AnyRef, U](r: T)(f: Node => U): U = node(r) match {
    case Some(node) => f(node)
    case None => throw new OcmException("Object not bound to node.")
  }

  protected[ocm] def node(r: AnyRef): Option[Node] =
    node2obj.value.find { case ((node, obj)) => obj == r } map (_._1)

  def identifier(r: AnyRef): String = withNode(r) { _.getIdentifier }

}