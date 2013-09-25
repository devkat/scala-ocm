package net.devkat.scalaocm
import javax.jcr.Repository
import javax.jcr.SimpleCredentials
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.DynamicVariable
import javax.jcr.Session
import javax.jcr.Node
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import net.devkat.scalaocm.annotation.JcrProperty

trait Ocm extends Logging {

  import Extensions._
  import Reflection._
  import ValueConversions._

  val currentSession = new DynamicVariable[Session](null)
  val nodes = new DynamicVariable[scala.collection.mutable.Map[AnyRef, Node]](null)

  implicit def jcrSession = currentSession.value match {
    case null => throw new RuntimeException("Not logged in to repository.")
    case s => s
  }

  def namespaceRegistry = jcrSession.getWorkspace.getNamespaceRegistry
  val scalaOcmNamespace = Namespace("socm", "http://scalaocm.org/ns")

  def transaction[T](repo: Repository)(f: => T): T = {
    val session = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    currentSession.withValue(session) {
      nodes.withValue(scala.collection.mutable.Map.empty[AnyRef, Node]) {
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

  protected def fromNode[T <: AnyRef](n: Node)(implicit m: Manifest[T]): T = {
    val r = newInstance[T]
    load(r, n)
    r
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

  private val classNameProperty = "class"

  def create[T <: AnyRef](path: Path)(implicit m: Manifest[T]): T = {
    val r = newInstance[T]
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    node.setProperty(scalaOcmNamespace.prefixed(classNameProperty), getClass.getName)
    nodes.value.put(r, node)
    r
  }

  protected def jcrPath(node: Node) = Path.parse(node.getPath)
  
  implicit def path(r: AnyRef) = withNode(r) { jcrPath _ }

  def identifier(r: AnyRef): String = withNode(r) { _.getIdentifier }

  def getJcrFields(symbol: ClassSymbol): Iterable[TermSymbol] =
    symbol.toType.members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) &&
        t.annotations.find(_.tpe == universe.typeOf[JcrProperty]).isDefined => t
    }

  protected def instanceMirror(r: AnyRef) =
    universe.runtimeMirror(getClass.getClassLoader).reflect(r)

  def getPropertyValue(node:Node, name: String): Option[Any] = {
    if (node.hasProperty(name)) {
      val prop = node.getProperty(name)
      val v = if (prop.isMultiple) {
        prop.getValues.toList map value2any _
      } else {
        value2any(prop.getValue)
      }
      Some(v)
    } else None
  }

  def load(r:AnyRef, node: Node) = {
    nodes.value.put(r, node)
    val mirror = instanceMirror(r)
    getJcrFields(mirror.symbol) foreach { field =>
      val name = fieldName(field)
      val value = getPropertyValue(node, name)
      logger.info("Loading property {}.{} := {}", node.getPath, name, value)
      //instanceMirror.reflectField(field).set(value.orNull)
      value foreach { v => setField(mirror.reflectField(field), v) }
    }
  }

  def setField(field: FieldMirror, value: Any): Unit

  def fieldName(field: TermSymbol) = field.name.decoded.trim

  def save(r: AnyRef) {
    withNode(r) { n =>

      def setProperty(name: String, v: Any) {
        v match {
          case null => n.setProperty(name, null.asInstanceOf[String])
          case l: List[_] => n.setProperty(name, list2values(l))
          case v => n.setProperty(name, any2value(v))
        }
        logger.info("Save property {}.{} = {}", jcrPath(n), name, getPropertyValue(n, name))
      }

      val mirror = instanceMirror(r)
      getJcrFields(mirror.symbol) foreach { field =>
        val v = getField(mirror.reflectField(field))
        setProperty(fieldName(field), v)
      }
    }
  }
  
  def getField(field:FieldMirror): Any
  
  protected[scalaocm] def node(r: AnyRef) = nodes.value.get(r)

  protected[scalaocm] def withNode[T <: AnyRef, U](r: T)(f: Node => U): U = node(r) match {
    case Some(node) => f(node)
    case None => throw new RuntimeException("Object not bound to node.")
  }

  def remove(r: AnyRef) {
    withNode(r) { _.remove() }
  }
  
}