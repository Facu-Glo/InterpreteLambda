import org.scalatest.flatspec.AnyFlatSpec
import Lexer.lexer

class LexerTest extends AnyFlatSpec {

  def testLexer(input: String, expected: String): Unit = {
    it should s"pass for input: $input" in {
      val lex = lexer.tokenizador(input)
      assert(lex.toString() == expected)
    }
  }

  testLexer("(λx.λy.y (λx.(x x) λx.(x x)))", "List(L_PAR, LAMBDA, EXPRESION(x), PUNTO, LAMBDA, EXPRESION(y), PUNTO, EXPRESION(y), ESPACIO, L_PAR, LAMBDA, EXPRESION(x), PUNTO, L_PAR, EXPRESION(x), ESPACIO, EXPRESION(x), R_PAR, ESPACIO, LAMBDA, EXPRESION(x), PUNTO, L_PAR, EXPRESION(x), ESPACIO, EXPRESION(x), R_PAR, R_PAR, R_PAR)")
  testLexer("(λx.λy.x y)","List(L_PAR, LAMBDA, EXPRESION(x), PUNTO, LAMBDA, EXPRESION(y), PUNTO, EXPRESION(x), ESPACIO, EXPRESION(y), R_PAR)" )
  testLexer("(λf.(f λx.λy.x) ((λx.λy.λf.((f x) y) a) b))", "List(L_PAR, LAMBDA, EXPRESION(f), PUNTO, L_PAR, EXPRESION(f), ESPACIO, LAMBDA, EXPRESION(x), PUNTO, LAMBDA, EXPRESION(y), PUNTO, EXPRESION(x), R_PAR, ESPACIO, L_PAR, L_PAR, LAMBDA, EXPRESION(x), PUNTO, LAMBDA, EXPRESION(y), PUNTO, LAMBDA, EXPRESION(f), PUNTO, L_PAR, L_PAR, EXPRESION(f), ESPACIO, EXPRESION(x), R_PAR, ESPACIO, EXPRESION(y), R_PAR, ESPACIO, EXPRESION(a), R_PAR, ESPACIO, EXPRESION(b), R_PAR, R_PAR)")
  testLexer("(λx.λx.(y x) z)", "List(L_PAR, LAMBDA, EXPRESION(x), PUNTO, LAMBDA, EXPRESION(x), PUNTO, L_PAR, EXPRESION(y), ESPACIO, EXPRESION(x), R_PAR, ESPACIO, EXPRESION(z), R_PAR)")
}


