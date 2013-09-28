package net.devkat.ocm

case class Path(val names:List[String], val absolute:Boolean) {
  
  def /(name:String) = Path(names ::: name :: Nil, absolute)
  
  override def toString = (absolute match {
    case true => "" :: names
    case false => names
  }) mkString "/"
  
}

object Path {
  val root = Path(Nil, true)

  def parse(s:String) = Path("""\/""".r.split("""^\/""".r.replaceFirstIn(s, "")).toList, s.startsWith("/"))
}
