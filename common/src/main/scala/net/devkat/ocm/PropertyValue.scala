package net.devkat.ocm

import javax.jcr.{Node, Value}
/*
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
*/
/*
sealed trait PropertyValue

object PropertyValue {
  case class simple[T](t: PropertyType.simple[T], v: T) extends PropertyValue
  case class optional[T](t: PropertyType.optional[T], v: Option[T]) extends PropertyValue
  case class multi[T](t: PropertyType.multi[T], v: Iterable[T]) extends PropertyValue
}
*/