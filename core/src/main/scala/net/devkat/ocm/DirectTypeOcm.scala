package net.devkat.ocm

import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.slf4j.Logging
import java.io.ByteArrayInputStream

trait DirectTypeMapper extends Mapper with Logging {

  self: SessionHolder =>

  import Reflection._
  import PropertyType._
  import PropertyAccessor._

  def writeProperty[T](r: AnyRef, name: String, t: PropertyType[T], value: T) {
    withNode(r) { node => setProperty(node, name, t, value) }
  }

  def readProperty[T](r: AnyRef, name: String, t: PropertyType[T]): T =
    withNode(r) { node => getProperty(node, name, t) }

  def setFieldValue(field: FieldMirror, value: Any) {
    field.set(value)
  }

  def getFieldValue[T](field: FieldMirror): T =
    field.get.asInstanceOf[T]

  def getFieldType(field: TermSymbol): PropertyType[_] = {
    val sig = field.typeSignature
    null.asInstanceOf[PropertyType[_]]
    /*
    TypeMapper.getPropertyType(sig) match {
      case Some(t) => t
      case t @ None => throw new OcmException(
        "Unsupported type %s for field '%s'".format(sig, fieldName(field)))
    }
    */
  }

}

object DirectTypeOcm extends Ocm with DirectTypeMapper