package Parser
import Lexer.{ESPACIO, EXPRESION, LAMBDA, L_PAR, PUNTO, R_PAR, Token}

object Parser {
    def parser(lista: List[Token]): Ast =
        val(ast,_)=parserAux(lista)
        ast

    private def parserAux(tokens:List[Token]):(Ast, List[Token]) = tokens match {
        case EXPRESION(nombre) :: resto => (Var(nombre),resto)

        case LAMBDA :: EXPRESION(nombre) :: PUNTO :: cuerpo =>
            val (ast, resto) = parserAux(cuerpo)
            (Abstr(nombre,ast),resto)

        case ESPACIO :: resto => parserAux(resto)

        case L_PAR :: resto =>
            val (arbolIzq,der) = parserAux(resto)
            val (arbolDer,argumento) = parserAux(der)
            argumento match{
                case R_PAR :: fin => (App(arbolIzq,arbolDer),fin)
            }
    }

    def unParser(arbol: Ast): String = arbol match {
        case Var(nombre) => nombre
        case Abstr(variable, cuerpo) => "λ" + variable + "." + unParser(cuerpo)
        case App(funcion, argumento) =>
            val expresion1 = unParser(funcion)
            val expresion2 = unParser(argumento)
            "(" + expresion1 + " " + expresion2 + ")"
    }
}

