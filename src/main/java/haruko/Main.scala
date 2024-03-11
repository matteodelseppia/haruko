package haruko

import haruko.implementation.lexer.Lexer

object Hello {
  def main(args: Array[String]): Unit = {
    val lexer = new Lexer("true false (def a 3) \"hello\" \n 2.0 -3.13 +4")
    println(lexer.getTokens())
  }
}
