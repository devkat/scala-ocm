package net.devkat.ocm

case class Namespace(prefix:String, uri:String) {
  
  def prefixed(name:String) = prefix + ":" + name
  
}