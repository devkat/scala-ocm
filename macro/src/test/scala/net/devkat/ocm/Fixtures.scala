package net.devkat.ocm

import net.devkat.ocm.macros._

@node
class AnnotatedFoo {
  //@property var name:String = "hi"
  var name: String = "hi"
}

/*
object Temp {
  val a = Block(
    List(
      ClassDef(
        Modifiers(),
        newTypeName("AnnotatedFoo"),
        List(),
        Template(
          List(
            Select(
              Ident(scala),
              newTypeName("AnyRef"))),
          emptyValDef,
          List(
            DefDef(
              Modifiers(),
              nme.CONSTRUCTOR,
              List(),
              List(List()),
              TypeTree(),
              Block(
                List(
                  Apply(
                    Select(
                      Super(
                        This(tpnme.EMPTY),
                        tpnme.EMPTY),
                      nme.CONSTRUCTOR),
                    List())),
                Literal(Constant(())))),
            ValDef(Modifiers(PRIVATE | MUTABLE | DEFAULTINIT),
              newTermName("_name"),
              Ident(newTypeName("String")),
              EmptyTree),
            DefDef(
              Modifiers(),
              newTermName("fieldName"),
              List(),
              List(),
              TypeTree(),
              Ident(newTermName("_name"))),
            DefDef(Modifiers(),
              newTermName("name_="),
              List(), List(List(ValDef(Modifiers(PARAM), newTermName("a"), Ident(newTypeName("String")), EmptyTree))), Select(Ident(newTermName("scala")), newTypeName("Unit")), Block(List(Apply(Ident(newTermName("println")), List(Apply(Select(Literal(Constant("Setting variable to %s")), newTermName("format")), List(Ident(newTermName("a"))))))), Assign(Ident(newTermName("_name")), Apply(Select(Ident(newTermName("a")), newTermName("$plus")), List(Literal(Constant(" changed"))))))))))), Literal(Constant(())))
}
*/