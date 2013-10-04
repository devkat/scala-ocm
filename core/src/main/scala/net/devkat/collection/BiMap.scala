package net.devkat.collection

/**
 * http://stackoverflow.com/questions/9850786/is-there-such-a-thing-as-bidirectional-maps-in-scala
 */
object BiMap {
  private[BiMap] trait MethodDistinctor
  implicit final object MethodDistinctor extends MethodDistinctor
}

case class BiMap[X, Y](map: Map[X, Y]) {
  def this(tuples: (X,Y)*) = this(tuples.toMap)
  private val reverseMap = map map (_.swap)
  require(map.size == reverseMap.size, "no 1 to 1 relation")
  def apply(x: X): Y = map(x)
  def apply(y: Y)(implicit d: BiMap.MethodDistinctor): X = reverseMap(y)
  val domain = map.keys
  val codomain = reverseMap.keys
}