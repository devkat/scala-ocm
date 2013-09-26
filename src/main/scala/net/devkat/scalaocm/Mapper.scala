package net.devkat.scalaocm

import scala.reflect.runtime.universe.FieldMirror
import com.typesafe.scalalogging.slf4j.Logging
import javax.jcr.Node
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import net.devkat.scalaocm.annotation.JcrProperty

trait Mapper extends Logging {
  
  self: SessionHolder =>
  
  import Reflection._
  import ValueConversions._
    
  def setField(field: FieldMirror, value: Any): Unit

  def getField(field:FieldMirror): Any
  
  protected def jcrPath(node: Node) = Path.parse(node.getPath)

  protected def fromNode[T <: AnyRef](n: Node)(implicit m: Manifest[T]): T = {
    node2obj.value.getOrElseUpdate(n, {
      val r = newInstance[T]
      load(r, n)
      r
    }).asInstanceOf[T]
  }

  protected def load(r:AnyRef, node: Node) = {
    val mirror = instanceMirror(r)
    getJcrFields(mirror.symbol) foreach { field =>
      val name = fieldName(field)
      val value = getPropertyValue(node, name)
      logger.info("Loading property {}.{} := {}", node.getPath, name, value)
      //instanceMirror.reflectField(field).set(value.orNull)
      value foreach { v => setField(mirror.reflectField(field), v) }
    }
  }
  
  protected def save(r:AnyRef) = {
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

  protected def getJcrFields(symbol: ClassSymbol): Iterable[TermSymbol] =
    symbol.toType.members.collect {
      case t: TermSymbol if (t.isVal || t.isVar) &&
        t.annotations.find(_.tpe == universe.typeOf[JcrProperty]).isDefined => t
    }

  protected def getPropertyValue(node:Node, name: String): Option[Any] = {
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

  protected def fieldName(field: TermSymbol) = field.name.decoded.trim

}