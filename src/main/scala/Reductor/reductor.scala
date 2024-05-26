package Reductor

import Parser.{Var,App,Abstr, Ast}

object reductor {
  def reductor(arbol: Any): Ast = arbol match {
    case App(Abstr(param, cuerpo),argumento) => sustituir(cuerpo,argumento,param)
    ////Me faltan los otros casos por ejemplo => ((expresion1 expresion2) argumento)
  }


  def sustituir(funcion: Ast, argumento: Ast, variable: String): Ast = funcion match {
    case Var(nombre) if nombre == variable => argumento
    case Var(nombre) => Var(nombre)
    case Abstr(ligada, cuerpo) if ligada == variable => Abstr(ligada, cuerpo)
    case Abstr(ligada, cuerpo) => Abstr(ligada, sustituir(cuerpo, argumento, variable))
    case App(e1, e2) => App(sustituir(e1, argumento, variable), sustituir(e2, argumento, variable))
  }

  def variablesLibres(ast: Any): Set[String] = ast match {
    case Var(nombre) => Set(nombre)
    case Abstr(variable,cuerpo) => variablesLibres(cuerpo) - variable
    case App(e1,e2) => variablesLibres(e1)|variablesLibres(e2)
  }

}


