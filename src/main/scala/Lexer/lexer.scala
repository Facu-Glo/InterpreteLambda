package Lexer
object lexer {
  
  def tokenizador(expresion: String):List[Token]={
    tokenizadorAux(expresion.split("").toList, List())
  }

  def tokenizadorAux(expresion:List[String],lista:List[Token]):List[Token]= expresion match{
    case Nil => lista.reverse
    case "λ" :: resto => tokenizadorAux(resto, LAMBDA :: lista)
    case "\\" :: resto => tokenizadorAux(resto, LAMBDA :: lista)
    case "." :: resto => tokenizadorAux(resto, PUNTO :: lista)
    case "(" :: resto => tokenizadorAux(resto, L_PAR :: lista)
    case ")" :: resto => tokenizadorAux(resto, R_PAR :: lista)
    case " " :: resto => tokenizadorAux(resto, ESPACIO :: lista)
    case x :: resto  => tokenizadorAux(resto, EXPRESION(x) :: lista)
  }
}
