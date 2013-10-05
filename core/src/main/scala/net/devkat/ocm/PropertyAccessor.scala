package net.devkat.ocm

import javax.jcr.Node
import javax.jcr.Session
import java.io.ByteArrayInputStream
import java.util.Calendar
import javax.jcr.Value
import scala.util.Try
import javax.jcr.PropertyType._
import net.devkat.ocm.PropertyType._
import org.apache.commons.io.IOUtils
import javax.jcr.Property
import scala.util.Success
import scala.util.Failure
import scala.reflect.runtime.universe._

sealed trait PropertyValue

object PropertyValue {
  case object missing extends PropertyValue
  case class single(v: Value) extends PropertyValue
  case class multiple(v: Iterable[Value]) extends PropertyValue

  def apply(node: Node, name: String): PropertyValue = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) multiple(p.getValues)
      else single(p.getValue)
    } else missing
  }
}

object PropertyAccessor {

  import javax.jcr.PropertyType._

  protected def createValue[T](jcrPropertyType: Int)(value: T)(implicit jcrSession: Session): Value = {
    lazy val factory = jcrSession.getValueFactory
    jcrPropertyType match {
      case BINARY => factory.createValue(factory.createBinary(new ByteArrayInputStream(value.asInstanceOf[Array[Byte]])))
      case BOOLEAN => factory.createValue(value.asInstanceOf[Boolean])
      case DATE => factory.createValue(value.asInstanceOf[Calendar])
      case DECIMAL => factory.createValue(new java.math.BigDecimal(value.asInstanceOf[BigDecimal].toDouble))
      case DOUBLE => factory.createValue(value.asInstanceOf[Double])
      case LONG => factory.createValue(value.asInstanceOf[Long])
      case t => factory.createValue(value.asInstanceOf[String], t)
    }
  }

  protected def typeName(jcrPropertyType: Int) =
    javax.jcr.PropertyType.nameFromValue(jcrPropertyType)

  protected def extractValue[T](jcrPropertyType: Int)(value: Value): T = {
    if (value.getType != jcrPropertyType) {
      throw new OcmException(s"Found property type ${typeName(value.getType)}, required type ${typeName(jcrPropertyType)}.")
    } else {
      (jcrPropertyType match {
        case BINARY => IOUtils.toByteArray(value.getBinary.getStream)
        case BOOLEAN => value.getBoolean
        case DATE => value.getDate
        case DECIMAL => value.getDecimal
        case DOUBLE => value.getDouble
        case LONG => value.getLong
        case _ => value.getString
      }).asInstanceOf[T]
    }
  }

  def setProperty[T](node: Node, name: String, t: PropertyType[T], v: T)(implicit jcrSession: Session): Unit = {
    def clear = if (node.hasProperty(name)) node.getProperty(name).remove()
    val create = createValue(t.jcrPropertyType) _
    t match {
      case _: simple[T] => node.setProperty(name, create(v))
      case _: optional[T] => v.asInstanceOf[Option[_]] match {
        case Some(w) => node.setProperty(name, create(w))
        case None => clear
      }
      case _: multi[T] => {
        val i = v.asInstanceOf[Iterable[_]]
        if (i.isEmpty) clear
        else node.setProperty(name, i.map(create).toArray)
      }
    }
  }

  def flatten[T](xs: Seq[Try[T]]): Try[Seq[T]] = {
    val (ss: Seq[Success[T]] @unchecked, fs: Seq[Failure[T]] @unchecked) =
      xs.partition(_.isSuccess)

    if (fs.isEmpty) Success(ss map (_.get))
    else Failure[Seq[T]](fs(0).exception) // Only keep the first failure
  }
  
  def getProperty[T](node: Node, name: String, t: PropertyType[T])(implicit jcrSession: Session): T = {
    import PropertyValue._
    val prop = PropertyValue(node, name)
    val extract = extractValue(t.jcrPropertyType) _
    t match {
      case _: simple[T] => prop match {
        case single(v) => extract(v)
        case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
        case missing => throw new OcmException(s"Property ${name} missing.")
      }
      case _: optional[T] => prop match {
        case single(v) => extract(v)
        case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
        case missing => None.asInstanceOf[T]
      }
      case _: multi[T] => prop match {
        case single(_) => throw new OcmException(s"Property ${name} is single.")
        //case multiple(v) => flatten(v.map(extract).toSeq).asInstanceOf[Try[T]]
        case multiple(v) => v.map(extract)
        case missing => throw new OcmException(s"Property ${name} missing.")
      }
    }
  }
}
