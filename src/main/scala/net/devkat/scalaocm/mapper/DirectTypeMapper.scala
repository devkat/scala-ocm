package net.devkat.scalaocm.mapper

import net.devkat.scalaocm.Ocm
import scala.reflect.runtime.universe._

trait DirectTypeMapper {
  
  self: Ocm =>
  
  def setField(field: FieldMirror, value:Any) {
    field.set(value)
  }
  
  def getField(field:FieldMirror): Any =
    field.get

}