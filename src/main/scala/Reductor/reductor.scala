package Reductor

import Parser.{Var,App,Abstr, Ast,Parser}

object reductor {
  def reductor(arbol: Any): Ast =
    //reductor CallByName
    val ast = conversionAlfa(arbol,variablesLibres(arbol))
    ast match {
      case Abstr(param, cuerpo) => Abstr(param,cuerpo)
      case Var(nombre) => Var(nombre)
      case App(e1,e2) => sustituir(e1, e2, "")
    }

  def sustituir(funcion: Ast, argumento: Ast, variable: String): Ast =
    funcion match {
    case Var(nombre) if nombre == variable => argumento
    case Var(nombre) => funcion
    case Abstr(ligada,cuerpo) if variable == "" => sustituir(cuerpo, argumento, ligada)
    case Abstr(ligada, cuerpo) => Abstr(ligada,sustituir(cuerpo, argumento, variable))
    case App(e1, e2) => App(sustituir(e1, argumento, variable),e2)
  }

  def variablesLibres(ast: Any): Set[String] = ast match {
    case Var(nombre) => Set(nombre)
    case Abstr(variable,cuerpo) => variablesLibres(cuerpo) - variable
    case App(e1,e2) => variablesLibres(e1)|variablesLibres(e2)
  }

  def conversionAlfa(arbol: Any, variableLi: Set[String]):Ast =
    arbol match {
      case Var(nombre) if variableLi.contains(nombre) => Var(nombre + "*")
      case Var(nombre) => Var(nombre)
      case Abstr(parametro, cuerpo) if variableLi.contains(parametro) =>
        cuerpo match{
          case App(e1,e2) => Abstr(parametro+ "*",App(conversionAlfa(e1,variableLi),conversionAlfa(e2,variableLi)))
          case _ => Abstr(parametro + "*", conversionAlfa(cuerpo, variableLi))
        }

      case Abstr(parametro, cuerpo) => Abstr(parametro,conversionAlfa(cuerpo, variableLi))
      case App(e1, e2) => App(conversionAlfa(e1, variableLi), e2)
    }
}

//*********************************************************
// ANTERIOR VERSION

//def reductor(arbol: Any): Ast =
//  //reductor CallByName
//  arbol match {
//    case Abstr(param, cuerpo) => Abstr(param, cuerpo)
//    case Var(nombre) => Var(nombre)
//    case App(e1, e2) => sustituir(e1, e2, "")
//  }
//
//def sustituir(funcion: Ast, argumento: Ast, variable: String): Ast =
//  funcion match {
//    case Var(nombre) if nombre == variable => argumento
//    case Var(nombre) => funcion
//    //case Abstr(ligada, cuerpo) if ligada == variable => funcion
//    case Abstr(ligada, cuerpo) if variable == "" => sustituir(cuerpo, argumento, ligada)
//    case Abstr(ligada, cuerpo) => Abstr(ligada, sustituir(cuerpo, argumento, variable))
//    case App(e1, e2) => App(sustituir(e1, argumento, variable), e2)
//  }
