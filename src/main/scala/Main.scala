import Lexer.lexer
import Parser.Parser
import Reductor.reductor

object Main {
  def main(args: Array[String]): Unit = {
    val str = "(λx.λx.(y x) z)"
    val lex = lexer.tokenizador(str)
    val pars= Parser.parser(lex)
    val unpar= Parser.parser(pars)
    val variablesLibres = reductor.variablesLibres(pars)
    val reduccion = reductor.reductor(pars)
    println(s"Input: $str")
    println(s"Lexer: $lex")
    println(s"Parser: $pars")
    println(s"Unparser: $unpar")
    println(s"Variables Libres: $variablesLibres")
    println(s"Reductor: ${Parser.parser(reduccion)}")
  }
}