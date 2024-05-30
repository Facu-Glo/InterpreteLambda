package Reductor

import Parser.{Var,App,Abstr, Ast,Parser}

object reductor {

  def callByValue(arbol: Any): Ast = {
    val nuevo = conversionAlfa(arbol, "", variablesLibres(arbol))
    nuevo match
      case Var(nombre) => Var(nombre)
      case Abstr(param, cuerpo) => Abstr(param, cuerpo)
      case App(e1, e2) =>
        callByValue(e1) match
          case Abstr(param, cuerpo) =>
            val valorArg = callByValue(e2)
            callByValue(sustituir(cuerpo, param, valorArg))
  }

  def callByName(arbol: Any): Ast = {
    val nuevo = conversionAlfa(arbol, "", variablesLibres(arbol))
    nuevo match
      case Var(nombre) => Var(nombre)
      case Abstr(param, cuerpo) => Abstr(param, cuerpo)
      case App(e1, e2) =>
        callByName(e1) match
          case Abstr(param, cuerpo) => callByName(sustituir(cuerpo, param, e2))
  }

  def sustituir(arbol: Ast, variable: String, argumento: Ast): Ast = {
    arbol match
      case Var(nombre) if nombre == variable => argumento
      case Var(nombre) => Var(nombre)
      case Abstr(param, cuerpo) if param == variable => Abstr(param, cuerpo)
      case Abstr(param, cuerpo) => Abstr(param, sustituir(cuerpo, variable, argumento))
      case App(e1, e2) => App(sustituir(e1, variable, argumento), sustituir(e2, variable, argumento))
  }
  
  def variablesLibresGraf(valor: Any):String = {
    val variables = variablesLibres(valor)
    if (variables.isEmpty) "{}" else "{"+ variables.reduce((x:String,y:String) => x + "," + y) + "}"
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

