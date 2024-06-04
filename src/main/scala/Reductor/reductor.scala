package Reductor

import Parser.{Var,App,Abstr, Ast,Parser}

object reductor {

  def reduccion(arbol:Any,estrategia: Ast => Ast):Ast = {
    val nuevo = conversionAlfa(arbol, "", variablesLibres(arbol))
    nuevo match
      case Var(nombre) => nuevo
      case Abstr(param, cuerpo) => nuevo
      case App(e1,e2) => estrategia(nuevo)
  }

  def callByValue(arbol: Ast):Ast = {
    arbol match
      case App(e1,e2) =>
        reduccion(e1, callByValue) match
          case Abstr(param,cuerpo) =>
            val valorArg = reduccion(e2,callByValue)
            reduccion(sustituir(cuerpo,param,valorArg),callByValue)
          case _ => arbol
      case _ => arbol
  }
  
  def callByName(arbol:Ast):Ast = {
    arbol match
      case App(e1,e2) =>
        reduccion(e1, callByName) match
          case Abstr(param, cuerpo) => reduccion(sustituir(cuerpo, param, e2),callByName)
          case _ => arbol
      case _ => arbol
  }
  def sustituir(arbol: Any, variable: String, argumento: Ast): Ast = {
    arbol match
      case Var(nombre) if nombre == variable => argumento
      case Var(nombre) => Var(nombre)
      case Abstr(param, cuerpo) if param == variable => Abstr(param,cuerpo)
      case Abstr(param, cuerpo) => Abstr(param, sustituir(cuerpo, variable, argumento))
      case App(e1, e2) => App(sustituir(e1, variable, argumento), sustituir(e2, variable, argumento))
  }

  def variablesLibres(ast: Any): Set[String] = ast match {
    case Var(nombre) => Set(nombre)
    case Abstr(variable,cuerpo) => variablesLibres(cuerpo) - variable
    case App(e1,e2) => variablesLibres(e1)|variablesLibres(e2)
  }
  
  def conversionAlfa(arbol:Any, parametro:String, variables:Set[String]):Ast=
    arbol match {
      case Var(nombre) if nombre == parametro => Var(nombre +"*")
      case Var(nombre) => Var(nombre)
      case Abstr(param,cuerpo) if variables.contains(param) =>
        cuerpo match
          case App(e1,e2) => Abstr(param+"*",App(conversionAlfa(e1,param,variables),conversionAlfa(e2,param,variables)))
          case _ => Abstr(param+"*",conversionAlfa(cuerpo,param,variables))
      case Abstr(param, cuerpo) => Abstr(param,conversionAlfa(cuerpo,parametro, variables))
      case App(e1, e2) => App(conversionAlfa(e1,parametro, variables), e2)
    }
}

