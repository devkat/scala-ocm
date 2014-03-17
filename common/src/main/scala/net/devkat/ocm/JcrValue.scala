import javax.jcr.{Node, PropertyType, Value}
import net.devkat.ocm.OcmException
import java.util.Calendar
import org.apache.commons.io.IOUtils
import scala.reflect.runtime.universe._

sealed trait JcrValue {
  def jcrValue: Value
}

abstract class AbstractJcrValue[T](value: Value, v: T) extends JcrValue {
  def jcrValue = value
}

object JcrValue {

  import PropertyType._
  /*
  // http://stackoverflow.com/questions/18499384/polymorphic-instantiation-in-scala-using-typetag-and-classtag
  def newInstance[T <: JcrValue : TypeTag](v: Value): T = {
    val tpe = typeOf[T]
    def fail = throw new IllegalArgumentException(s"Cannot instantiate $tpe")
    val ctor = tpe.member(nme.CONSTRUCTOR) match {
      case symbol: TermSymbol =>
        symbol.alternatives.collectFirst {
          case constr: MethodSymbol if (constr.paramss match {
            case List(List(param)) if (param.typeSignature =:= typeOf[Value]) => true
            case _ => false
          }) => constr
        } getOrElse fail
      case NoSymbol => fail
    }
    val classMirror = typeTag[T].mirror.reflectClass(tpe.typeSymbol.asClass)
    classMirror.reflectConstructor(ctor).apply(v).asInstanceOf[T]
  }
  
  implicit def extract[T <: JcrValue : TypeTag](v: Value): T = newInstance[T](v)
*/
  
  protected def typeName(jcrPropertyType: Int) =
    javax.jcr.PropertyType.nameFromValue(jcrPropertyType)

  implicit def value2jcrValue[T <: JcrValue : TypeTag](v: Value): T = {
    val reqType = jcrType[T]
    if (v.getType != reqType) {
      throw new OcmException(s"Found property type ${typeName(v.getType)}, required type ${typeName(reqType)}.")
    } else {
      (v.getType match {
        case BINARY => binary(v)
        case BOOLEAN => boolean(v)
        case DATE => date(v)
        case DECIMAL => decimal(v)
        case DOUBLE => double(v)
        case LONG => long(v)
        case NAME => name(v)
        case PATH => path(v)
        case REFERENCE => reference(v)
        case STRING => string(v)
        case URI => uri(v)
        case WEAKREFERENCE => weakReference(v)
      }).asInstanceOf[T]
    }
  }
  
  implicit def jcrValue2value(t: JcrValue) = t.jcrValue

  case class binary(v: Value) extends AbstractJcrValue[Array[Byte]](v, IOUtils.toByteArray(v.getBinary.getStream))
  case class boolean(v: Value) extends AbstractJcrValue[Boolean](v, v.getBoolean)
  case class date(v: Value) extends AbstractJcrValue[Calendar](v, v.getDate)
  case class decimal(v: Value) extends AbstractJcrValue[BigDecimal](v, v.getDecimal)
  case class double(v: Value) extends AbstractJcrValue[Double](v, v.getDouble)
  case class long(v: Value) extends AbstractJcrValue[Long](v, v.getLong)
  case class name(v: Value) extends AbstractJcrValue[String](v, v.getString)
  case class path(v: Value) extends AbstractJcrValue[String](v, v.getString)
  case class reference(v: Value) extends AbstractJcrValue[String](v, v.getString)
  case class string(v: Value) extends AbstractJcrValue[String](v, v.getString)
  case class uri(v: Value) extends AbstractJcrValue[String](v, v.getString)
  case class weakReference(v: Value) extends AbstractJcrValue[String](v, v.getString)

  def jcrType[T <: JcrValue: TypeTag]: Int = {
    val t = typeOf[T]
    if (t =:= typeOf[binary]) BINARY else if (t =:= typeOf[boolean]) BOOLEAN else if (t =:= typeOf[date]) DATE else if (t =:= typeOf[decimal]) DECIMAL else if (t =:= typeOf[double]) DOUBLE else if (t =:= typeOf[long]) LONG else if (t =:= typeOf[name]) NAME else if (t =:= typeOf[path]) PATH else if (t =:= typeOf[reference]) REFERENCE else if (t =:= typeOf[string]) STRING else if (t =:= typeOf[uri]) URI else if (t =:= typeOf[weakReference]) WEAKREFERENCE else
      throw new OcmException(s"Unsupported type ${t}")
  }
}

sealed trait ValueExtractor

object ValueExtractor {
  case object missing extends ValueExtractor
  case class single(v: Value) extends ValueExtractor
  case class multiple(v: Iterable[Value]) extends ValueExtractor

  def apply(node: Node, name: String): ValueExtractor = {
    if (node.hasProperty(name)) {
      val p = node.getProperty(name)
      if (p.isMultiple) multiple(p.getValues)
      else single(p.getValue)
    } else missing
  }
}

case class Simple[T](v: T) {
  def get: T = v
}

sealed trait PropertyAccessor[C[_]] {
  import javax.jcr.PropertyType._
  import JcrValue._

  def read[T <: JcrValue: TypeTag](node: Node, name: String): C[T]
  def write[T <: JcrValue: TypeTag](node: Node, name: String, v: C[T]): Unit

  protected def extractValue[T <: JcrValue : TypeTag](value: Value): T = value2jcrValue(value)

}

object PropertyAccessor {
  import ValueExtractor._
  import JcrValue._
  import Simple._

  implicit val simple = new PropertyAccessor[Simple] {

    def read[T <: JcrValue: TypeTag](node: Node, name: String): Simple[T] = {
      val extract = extractValue[T] _
      ValueExtractor(node, name) match {
        case single(v) => Simple(extract(v))
        case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
        case missing => throw new OcmException(s"Property ${name} missing.")
      }
    }

    def write[T <: JcrValue: TypeTag](node: Node, name: String, v: Simple[T]) {
      node.setProperty(name, v.get)
    }
  }

  implicit val optional = new PropertyAccessor[Option] {

    def read[T <: JcrValue: TypeTag](node: Node, name: String): Option[T] = {
      val extract = extractValue[T] _
      ValueExtractor(node, name) match {
        case single(v) => Some(extract(v))
        case multiple(_) => throw new OcmException(s"Property ${name} is multiple.")
        case missing => None.asInstanceOf[Option[T]]
      }
    }

    def write[T <: JcrValue: TypeTag](node: Node, name: String, v: Option[T]) {
      def clear = if (node.hasProperty(name)) node.getProperty(name).remove()
      v match {
        case Some(w) => node.setProperty(name, w)
        case None => clear
      }
    }
  }

  def read[C[_]: PropertyAccessor, T <: JcrValue: TypeTag](node: Node, name: String): C[T] =
    implicitly[PropertyAccessor[C]].read[T](node, name)

  def write[C[_]: PropertyAccessor, T <: JcrValue: TypeTag](node: Node, name: String, v: C[T]): Unit =
    implicitly[PropertyAccessor[C]].write[T](node, name, v)

}
