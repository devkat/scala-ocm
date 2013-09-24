package net.devkat.scalaocm

import javax.jcr.Node
import Path._
import Extensions.toIterator
import net.devkat.scalaocm.annotations.JcrProperty
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.slf4j.Logging

trait JcrNode[T <: JcrNode[T]] extends Logging {

  self: T =>

  import Ocm._
  import Extensions._
  import ValueConversions._

  protected def jcrPath(node: Node) = Path.parse(node.getPath)

  def jcrPath: Path = withNode { n => jcrPath(n) }

  private[scalaocm] var jcrNode: Option[Node] = None

  def identifier = withNode { _.getIdentifier }

  def getJcrFields(symbol: ClassSymbol): Iterable[TermSymbol] =
    symbol.toType.members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) &&
          t.annotations.find(_.tpe == universe.typeOf[JcrProperty]).isDefined => t
    }

  protected def instanceMirror()(implicit m: Manifest[T]) = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    mirror.reflect(this)
  }

  def load(node: Node)(implicit m: Manifest[T]) = {
    jcrNode = Some(node)
    getJcrFields(instanceMirror.symbol) foreach { field =>
      val name = fieldName(field)
      val value = getPropertyValue(name)
      logger.info("Loading property {}.{} := {}", node.getPath, name, value)
      //instanceMirror.reflectField(field).set(value.orNull)
      value foreach { v => instanceMirror.reflectField(field).set(v) }
    }
  }

  def fieldName(field: TermSymbol) = field.name.decoded.trim

  def getPropertyValue(name: String): Option[Any] = withNode { node =>
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

  def save()(implicit m: Manifest[T]) {
    getJcrFields(instanceMirror.symbol) foreach { field =>
      val v = instanceMirror.reflectField(field).get
      setProperty(fieldName(field), v)
    }
  }

  def setProperty(name: String, v: Any) { withNode { n =>
    v match {
      case null => n.setProperty(name, null.asInstanceOf[String])
      case l: List[_] => n.setProperty(name, list2values(l))
      case v => n.setProperty(name, any2value(v))
    }
    logger.info("Save property {}.{} = {}", jcrPath, name, getPropertyValue(name))
  }}

  protected def newInstance(implicit m: Manifest[T]): T = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val classSymbol = universe.typeOf[T].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val ctor = universe.typeOf[T].declaration(universe.nme.CONSTRUCTOR).asMethod
    val ctorMirror = classMirror.reflectConstructor(ctor)
    ctorMirror().asInstanceOf[T]
  }

  protected[scalaocm] def withNode[T](f: Node => T): T = jcrNode match {
    case Some(node) => f(node)
    case None => throw new RuntimeException("Record not bound to node.")
  }

  def remove() {
    withNode { _.remove() }
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

