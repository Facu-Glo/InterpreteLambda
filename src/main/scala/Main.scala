import Lexer.lexer
import Parser.Parser
import Reductor.reductor

object Main {
  def main(args: Array[String]): Unit = {
    val str = "(λx.λy.(x y) y)"  //ESTA PRUEBA EL CALL BY NAME NO LO HACE BIEN --> ((λc.c λa.λb.b) ((λa.λb.λf.((f a) b) p) q))
    val lex = lexer.tokenizador(str)
    val pars= Parser.parser(lex)
    val unpar= Parser.parser(pars)
    val variablesLibres = reductor.variablesLibres(pars)
    val reduccion = reductor.reductor(pars)
    println(s"---> Input: $str")
    println(s"---> Conversion alfa: ${Parser.parser(reductor.conversionAlfa(pars,variablesLibres))}")
    println(s"Lexer: $lex")
    println(s"Parser: $pars")
    println(s"Unparser: $unpar")
    println(s"Variables Libres: $variablesLibres")
    println(s"Reductor: ${Parser.parser(reduccion)}")
  }
}