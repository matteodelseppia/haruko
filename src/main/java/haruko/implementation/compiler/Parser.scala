package haruko.implementation.compiler

import haruko.implementation.lexer.{Lexeme, Token}
import scala.collection.mutable.ArrayBuffer


object ParserExceptions {
  case class DefMissingSymbolException(msg: String, info: Token) extends Exception(msg)
  case class MissingRightParent(msg: String, info: Token) extends Exception(msg)
  case class MissingDefaultInCond(msg: String, info: Token) extends Exception(msg)
}


class Parser(val tokens: Iterator[Token]) {
  private var current_token = tokens.next()
  private var current_lexeme = Lexeme.EOF
  private var program_body = ArrayBuffer.empty

  private val lexeme_function_map: Map[Lexeme, () => Expression] =
    Map(
      Lexeme.DEF -> parse_def,
      Lexeme.DEFN -> parse_defn,
      Lexeme.IF -> parse_if,
      Lexeme.COND -> parse_cond,
      Lexeme.LET -> parse_let,
      Lexeme.DO -> parse_do,
      Lexeme.IDENT -> parse_call,
      Lexeme.JVMCALL -> parse_call)


  while (!matchLexeme(Lexeme.EOF)) {
    parse_expression()
    nextToken()
  }

  private def nextToken(): Unit = {
    current_token = tokens.next()
    current_lexeme = current_token.lexeme
  }

  private def matchLexeme(lex: Lexeme): Boolean = {
    current_lexeme.equals(lex)
  }

  private def consume(lex: Lexeme, e: Exception): Unit = {
    if (!matchLexeme(lex)) throw e
    nextToken()
  }

  private def parse_expression() : Expression = {
    current_lexeme match {
      case Lexeme.LEFT_PARENT =>
        nextToken()
        val parse_method = lexeme_function_map(current_lexeme)
        parse_method()

      case Lexeme.CONST_D |
           Lexeme.CONST_L |
           Lexeme.CONST_S |
           Lexeme.TRUE |
           Lexeme.FALSE |
           Lexeme.NIL => new ConstExpression(current_token)

      case _ => new SymExpression(current_token)
    }
  }

  private def parse_def() : DefExpression = {
    nextToken()
    val identifier = current_token
    consume(
      Lexeme.IDENT,
      ParserExceptions.DefMissingSymbolException("Expected identifier after `def`, found: ", identifier))
    val assigned = parse_expression()
    nextToken()
    val def_expr = new DefExpression(identifier, assigned)
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw ParserExceptions.MissingRightParent("Missing parenthesis at the end of `def` expression, found: ", current_token)
    def_expr
  }

  private def parse_defn() : DefnExpression = {

  }

  private def parse_if() : IfExpression = {
    nextToken()
    val branchingExpr = parse_expression()
    nextToken()
    val ifTrue = parse_expression()
    nextToken()
    val ifFalse = parse_expression()
    nextToken()
    val ifExpr = new IfExpression(branchingExpr, ifTrue, ifFalse)
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw ParserExceptions.MissingRightParent("Missing parenthesis at the end of `if` expression, found: ", current_token)
    ifExpr
  }

  private def parse_cond() : CondExpression = {
    nextToken()
    var args: List[Expression] = List.empty
    while (current_lexeme != Lexeme.RIGHT_PARENT) {
      args = args :+ parse_expression()
      nextToken()
    }
    
    if (args.length % 2 == 0)
      throw ParserExceptions.MissingDefaultInCond("Missing default expression in cond ", current_token)
    
    val condExpression = new CondExpression(args)
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw ParserExceptions.MissingRightParent("Missing parenthesis at the end of `cond` expression, found: ", current_token)
    condExpression
  }

  private def parse_let() : LetExpression = {
    
  }

  private def parse_do() : DoExpression = {

  }

  private def parse_call() : FnCallExpression = {

  }
}
