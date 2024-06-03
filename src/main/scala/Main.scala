import scala.io.StdIn
import Lexer.lexer
import Parser.{Ast, Parser}
import Reductor.reductor

object Main {
  def main(args: Array[String]): Unit = {
    println("\nIntérprete de Cálculo Lambda\n")
    println("-> Puede utilizar \\ para el símbolo λ")
    println("-> Se requiere paréntesis para indicar la aplicación.")
    println("-> 'exit' para salir.\n")
    interfaz(reductor.callByName,"reduccion")
  }

  def interfaz(fEstrategia: Any => Ast, modo: String): Unit = {
    print("Input> ")
    val input: String = StdIn.readLine()
    input match
      case "exit" => println("Saliendo")
      case x: String if x.startsWith("set") =>
        val parte = x.split(" ")
        val (estrategia, modo) = parte(1) match
          case "call-by-value" =>
            println("Estrategia de reducción: Call-by-value")
            (reductor.callByValue, "reduccion")
          case "call-by-name" =>
            println("Estrategia de reducción: Call-by-name")
            (reductor.callByName, "reduccion")
          case "free-variables" =>
            println("Modo seleccionado: Ver variables libres")
            (fEstrategia, "variables")
        interfaz(estrategia, modo)
      case expresion: String =>
        val tokens = lexer.tokenizador(expresion)
        val parser = Parser.parser(tokens)
        modo match
          case "reduccion" => println(s"Reduccion-β: ${Parser.parser(fEstrategia(parser))}")
          case "variables" =>
            reductor.variablesLibres(parser) match
              case x if x.nonEmpty=> println(s"Conjunto de variables libres: {${x.reduce((x:String,y:String) => x + "," + y)}}")
              case _ => println("{}")
        interfaz(fEstrategia, modo)
  }

}