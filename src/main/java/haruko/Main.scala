package haruko

import haruko.implementation.lexer.Lexer

object Main {
  def main(args: Array[String]): Unit = {
    val lexer = new Lexer("true false (def aprint 3) \"hello\" \n -2.0 ^java/lang/Math  ")
    println(lexer.getTokens)
  }
}
