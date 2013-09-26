package net.devkat.scalaocm

import scala.reflect.runtime.universe._

trait DirectTypeMapper extends Mapper {
  
  self: SessionHolder =>
  
  def setField(field: FieldMirror, value:Any) {
    field.set(value)
  }
  
  def getField(field:FieldMirror): Any =
    field.get

}

object DirectTypeOcm extends Ocm with DirectTypeMapper