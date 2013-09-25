package net.devkat.scalaocm.mapper

import scala.reflect.runtime.universe._

trait Mapper {

  def setField(field: FieldMirror, value: Any): Unit

  def getField(field:FieldMirror): Any
  
}