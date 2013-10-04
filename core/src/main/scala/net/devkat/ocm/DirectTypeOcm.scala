package net.devkat.ocm

import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.slf4j.Logging

trait DirectTypeMapper extends Mapper with Logging {

  self: SessionHolder =>

  import Reflection._

  def writeProperty[T](r: Any, name: String, tpe: PropertyType[T], value: T) {
    println(s"Writing property: ${r.getClass.getName}.${name} = ${value}")
  }
  
  def readProperty[T](r: Any, name: String, tpe: PropertyType[T]): T = {
    println(s"Reading property: ${r.getClass.getName}.${name}")
    null.asInstanceOf[T]
  }

  def setFieldValue(field: FieldMirror, value: Any) {
    field.set(value)
  }

  def getFieldValue[T](field: FieldMirror): T =
    field.get.asInstanceOf[T]

  def getFieldType(field: TermSymbol): PropertyType[_] = {
    val sig = field.typeSignature
    TypeMapper.getPropertyType(sig) match {
      case Some(t) => t
      case t @ None => throw new OcmException(
          "Unsupported type %s for field '%s'".format(sig, fieldName(field)))
    }
  }

}

object DirectTypeOcm extends Ocm with DirectTypeMapper