package Parser

sealed trait Ast
case class Var(nombre: String) extends Ast
case class Abstr(variable:String, cuerpo:Ast) extends Ast
case class App(funcion:Ast, argumento:Ast) extends Ast