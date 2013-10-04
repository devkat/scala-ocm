package net.devkat.ocm

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object Reflection {

  protected[ocm] def newInstance[T]()(implicit m: Manifest[T]): T = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val classSymbol = universe.typeOf[T].typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val ctor = universe.typeOf[T].declaration(universe.nme.CONSTRUCTOR).asMethod
    val ctorMirror = classMirror.reflectConstructor(ctor)
    ctorMirror().asInstanceOf[T]
  }

  protected[ocm] def newInstance(t: Type)(args: Any*): Any = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val classSymbol = t.typeSymbol.asClass
    val classMirror = mirror.reflectClass(classSymbol)
    val ctor = t.declaration(universe.nme.CONSTRUCTOR).asMethod
    val ctorMirror = classMirror.reflectConstructor(ctor)
    ctorMirror(args)
  }

  protected[ocm] def instanceMirror(r: AnyRef) =
    universe.runtimeMirror(getClass.getClassLoader).reflect(r)

  protected[ocm] def fieldName(field: TermSymbol) = field.name.decoded.trim

}