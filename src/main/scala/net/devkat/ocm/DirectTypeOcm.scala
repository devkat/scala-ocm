package net.devkat.ocm

import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.slf4j.Logging

trait DirectTypeMapper extends Mapper with Logging {

  self: SessionHolder =>

  import Reflection._

  def setFieldValue(field: FieldMirror, value: Any) {
    field.set(value)
  }

  def getFieldValue(field: FieldMirror): Any =
    field.get

  def getFieldType(field: TermSymbol): Type = {
    val sig = field.typeSignature
    TypeMapper.getPropertyType(sig) match {
      case Some(t) => t
      case t @ None => throw new OcmException(
          "Unsupported type %s for field '%s'".format(sig, fieldName(field)))
    }
  }

}

object DirectTypeOcm extends Ocm with DirectTypeMapper