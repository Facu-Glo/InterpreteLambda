package Lexer
sealed trait Token
case object LAMBDA extends Token
case object PUNTO extends Token
case object L_PAR extends Token
case object R_PAR extends Token
case class EXPRESION(nombre:String) extends Token
case object ESPACIO extends Token
