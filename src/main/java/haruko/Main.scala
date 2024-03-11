package haruko

import haruko.implementation.lexer.Lexer

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new Lexer("true false (def a 3) \"hello\" \n -2.0 ")
    println(lexer.getTokens)
  }
}
