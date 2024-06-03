package Parser
import Lexer.{ESPACIO, EXPRESION, LAMBDA, L_PAR, PUNTO, R_PAR, Token}

object Parser {
    def parser[A](incognita: A): Any = incognita match{
        case lista : List[Token] => 
            val (ast,_) = parserAux(lista)
            ast
        case arbol: Ast => unParser(arbol)   
    }

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

    private def unParser(arbol: Any): String = arbol match {
        case Var(nombre) => nombre
        case Abstr(variable, cuerpo) => "λ" + variable + "." + unParser(cuerpo)
        case App(funcion, argumento) =>
            val expresion1 = unParser(funcion)
            val expresion2 = unParser(argumento)
            "(" + expresion1 + " " + expresion2 + ")"
    }

}

