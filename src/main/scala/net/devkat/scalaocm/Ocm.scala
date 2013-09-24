package net.devkat.scalaocm
import javax.jcr.Repository
import javax.jcr.SimpleCredentials
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.DynamicVariable
import javax.jcr.Session
import javax.jcr.Node

object Ocm extends Logging {
  
  import Extensions._
  import Reflection._

  val currentSession = new DynamicVariable[Session](null)

  def jcrSession = currentSession.value match {
    case null => throw new RuntimeException("Not logged in to repository.")
    case s => s
  }

  def namespaceRegistry = jcrSession.getWorkspace.getNamespaceRegistry
  val scalaOcmNamespace = Namespace("ljr", "http://liftweb.net/record/jcr")

  def withRepo[T](repo: Repository)(f: => T): T = {
    val session = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    currentSession.withValue(session) {
      
      if (!namespaceRegistry.getURIs.contains(scalaOcmNamespace.uri))
        namespaceRegistry.registerNamespace(scalaOcmNamespace.prefix, scalaOcmNamespace.uri)

      try {
        val user = session.getUserID
        val name = repo.getDescriptor(Repository.REP_NAME_DESC)
        logger.info("Logged in as %s to a %s repository.".format(user, name))
        val ret = f
        session.save()
        ret
      } finally {
        session.logout()
      }
    }
  }

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

  protected def fromNode[T <: JcrNode[T]](n: Node)(implicit m:Manifest[T]): T = {
    val r = newInstance[T]
    r.load(n)
    r
  }

  def getNodes[T <: JcrNode[T]](path: Path)(implicit m: Manifest[T]): Seq[T] =
    findNodes(path).map(fromNode[T] _).toSeq
    
  def getNode[T <: JcrNode[T]](path: Path)(implicit m: Manifest[T]): Option[T] =
    getNodes(path).headOption

  def parentOf[T <: JcrNode[T]](child: JcrNode[_])(implicit m: Manifest[T]): Option[T] = child withNode { n =>
    val parent = n.getParent
    parent.getPath() match {
      case "" => None
      case p => Some(fromNode(parent))
    }
  }

  private val classNameProperty = "class"

  def create[T <: JcrNode[T]](path: Path)(implicit m:Manifest[T]): T = {
    val r = newInstance[T]
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    node.setProperty(scalaOcmNamespace.prefixed(classNameProperty), getClass.getName)
    r.jcrNode = Some(node)
    r
  }

}