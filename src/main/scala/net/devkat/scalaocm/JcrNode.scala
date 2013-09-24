package net.devkat.scalaocm

import javax.jcr.Node
import Path._
import Extensions.toIterator
import net.devkat.scalaocm.annotations.JcrProperty
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.slf4j.Logging

trait JcrNode[T <: JcrNode[T]] extends Mapper[T] with Logging {

  self: T =>

  import Ocm._
  import Extensions._

  protected def jcrPath(node: Node) = Path.parse(node.getPath)

  def jcrPath: Path = withNode { n => jcrPath(n) }

  private[scalaocm] var jcrNode: Option[Node] = None

  def identifier = withNode { _.getIdentifier }

  def insertAt(path: Path): T = {
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    jcrNode = Some(node)
    this
  }

  def getJcrFields()(implicit m: Manifest[T]): Iterable[TermSymbol] =
    universe.typeOf[T].members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) && t.annotations.find(_.tpe == universe.typeOf[JcrProperty]).isDefined => t
    }

  def getJcrFields(symbol: ClassSymbol): Iterable[TermSymbol] =
    symbol.toType.members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) && t.annotations.find(_.tpe == universe.typeOf[JcrProperty]).isDefined => t
    }

  def load(node: Node)(implicit m: Manifest[T]) = {
    jcrNode = Some(node)
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val instanceMirror = mirror.reflect(this)
    getJcrFields(instanceMirror.symbol) foreach { field =>
      val value = getPropertyValue(field)
      logger.info("Loading property {}.{} := {}", node.getPath, fieldName(field), value)
      instanceMirror.reflectField(field).set(value.orNull)
    }
  }

  def fieldName(field: TermSymbol) = field.name.decoded.trim

  def getProperty(field: TermSymbol) = withNode { n =>
    n.getProperty(fieldName(field))
  }

  def getPropertyValue(field: TermSymbol): Option[Any] = withNode { node =>
    if (node.hasProperty(fieldName(field))) {
      val prop = getProperty(field)
      val v = if (prop.isMultiple) {
        prop.getValues.toList map value2any _
      } else {
        value2any(prop.getValue)
      }
      Some(v)
    } else None
  }

  val classNameProperty = "class"

  def save()(implicit m: Manifest[T]) {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val instanceMirror = mirror.reflect(this)
    getJcrFields(instanceMirror.symbol) foreach { field => saveProperty(instanceMirror, field) }
    withNode { n =>
      n.setProperty(scalaOcmNamespace.prefixed(classNameProperty), getClass.getName)
    }
  }

  def saveProperty(instanceMirror: InstanceMirror, field: TermSymbol) {
    withNode { n =>
      val v = instanceMirror.reflectField(field).get
      setProperty(fieldName(field), v)
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

  def find(path: Path)(implicit m: Manifest[T]): Seq[T] = {
    findNodes(path).map(fromNode _).toSeq
  }

  def newInstance(implicit m: Manifest[T]): T = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val classSymbol = universe.typeOf[T].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val ctor = universe.typeOf[T].declaration(universe.nme.CONSTRUCTOR).asMethod
    val ctorMirror = classMirror.reflectConstructor(ctor)
    ctorMirror().asInstanceOf[T]
  }

  def fromNode(n: Node)(implicit m: Manifest[T]): T = {
    val r = newInstance
    r.load(n)
    r
  }

  def withNode[T](f: Node => T): T = jcrNode match {
    case Some(node) => f(node)
    case None => throw new RuntimeException("Record not bound to node.")
  }

  def remove() {
    withNode { _.remove() }
  }

  def parentOf(child: JcrNode[_])(implicit m: Manifest[T]): Option[T] = child withNode { n =>
    val parent = n.getParent
    parent.getPath() match {
      case "" => None
      case p => Some(fromNode(parent))
    }
  }

  /*
  def getReferences[R <: NodeRecord[R]]: Iterator[(T, TypedField[_])] = withNode { node =>
    node.getReferences map { property =>
      val refRecord = fromNode(property.getParent)
      val field = refRecord.fieldByName(property.getName)
      (refRecord, field.asInstanceOf[TypedField[_]])
    }
  }
  */

}

