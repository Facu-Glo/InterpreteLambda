import org.scalatest.flatspec.AnyFlatSpec
import Parser.Parser
import Lexer.lexer

class ParserTest extends AnyFlatSpec {

    def testParse(input: String, expected: String): Unit = {
        it should s"parse correctly: $input" in {
        val lex = lexer.tokenizador(input)
        val parsed = Parser.parser(lex)

        assert(parsed.toString() == expected)
        }
    }

    def testUnparse(input: String): Unit = {
        it should s"unparse correctly: $input" in {
        val lex = lexer.tokenizador(input)
        val parsed = Parser.parser(lex)
        val unparsed = Parser.parser(parsed)

        assert(input == unparsed)
        }
    }

   
    testParse("(λx.λx.(y x) z)","App(Abstr(x,Abstr(x,App(Var(y),Var(x)))),Var(z))")
    testUnparse("(λx.λy.y (λx.(x x) λx.(x x)))")
    testUnparse("(λx.λy.x y)")
    testUnparse("(λf.(f λx.λy.x) ((λx.λy.λf.((f x) y) a) b))")
    testUnparse("(λx.λx.(y x) z)")
}


