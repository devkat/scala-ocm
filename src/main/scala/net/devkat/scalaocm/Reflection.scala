package net.devkat.scalaocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object Reflection {

  def newInstance[T]()(implicit m:Manifest[T]): T = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val classSymbol = universe.typeOf[T].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val ctor = universe.typeOf[T].declaration(universe.nme.CONSTRUCTOR).asMethod
    val ctorMirror = classMirror.reflectConstructor(ctor)
    ctorMirror().asInstanceOf[T]
  }

}