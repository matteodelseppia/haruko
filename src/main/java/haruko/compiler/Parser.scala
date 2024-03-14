package haruko.compiler


case class MissingSymbolException(msg: String) extends Exception(msg)
case class MissingRightParent(msg: String) extends Exception(msg)
case class MissingArguments(msg: String) extends Exception(msg)
case class MissingFnName(msg: String) extends Exception(msg)


class Parser(val tokens: Iterator[Token]) {
  private var currentToken: Token = null
  private var currentLexeme: Lexeme = null
  private var programBody: List[Expression] = List.empty

  private val lexemeToFunction: Map[Lexeme, () => Expression] =
    Map(
      Lexeme.DEF -> parseDef,
      Lexeme.DEFN -> parseDefn,
      Lexeme.IF -> parseIf,
      Lexeme.LET -> parseLet,
      Lexeme.DO -> parseDo,
      Lexeme.IDENT -> parseCall,
      Lexeme.COMPOSE -> parseCompose,
      Lexeme.RIGHT_PARENT -> skipForm)

  def getProgramBody: List[Expression] = {
    programBody
  }

  private def nextToken(): Unit = {
    currentToken = tokens.next()
    currentLexeme = currentToken.lexeme
  }

  private def matchLexeme(lex: Lexeme): Boolean = {
    currentLexeme.equals(lex)
  }

  private def consume(lex: Lexeme, e: Exception): Unit = {
    if (!matchLexeme(lex)) throw e
    nextToken()
  }

  private def skipForm(): Expression = {
    new DoExpression(List.empty)
  }

  private def parseExpression(): Expression = {
    currentLexeme match {
      case Lexeme.LEFT_PARENT =>
        nextToken()
        val parse_method = lexemeToFunction(currentLexeme)
        parse_method()

      case Lexeme.CONST_D |
           Lexeme.CONST_L |
           Lexeme.CONST_S |
           Lexeme.TRUE |
           Lexeme.FALSE |
           Lexeme.NIL => new ConstExpression(currentToken)

      case Lexeme.IDENT | Lexeme.SKIP => new SymExpression(currentToken)
    }
  }

  private def parseDef(): DefExpression = {
    nextToken()
    val identifier = currentToken
    consume(
      Lexeme.IDENT,
      MissingSymbolException("Expected identifier after `def`, found: " + identifier))
    val assigned = parseExpression()
    nextToken()
    val def_expr = new DefExpression(identifier, assigned)
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `def` expression, found: " + currentToken)
    def_expr
  }

  private def parseIf(): IfExpression = {
    nextToken()
    val branchingExpr = parseExpression()
    nextToken()
    val ifTrue = parseExpression()
    nextToken()
    val ifFalse = parseExpression()
    nextToken()
    val ifExpr = new IfExpression(branchingExpr, ifTrue, ifFalse)
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `if` expression, found: " + currentToken)
    ifExpr
  }

  private def parseCall(): FnCallExpression = {
    val function = currentToken
    nextToken()
    var args: List[Expression] = List.empty
    while (currentLexeme != Lexeme.RIGHT_PARENT) {
      args = args :+ parseExpression()
      nextToken()
    }

    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `cond` expression, found: " + currentToken)
    new FnCallExpression(function, args)
  }

  private def parseDefn(): DefnExpression = {
    nextToken()
    val function = currentToken
    consume(Lexeme.IDENT, MissingFnName("Missing function name after `def`, found: " + currentToken))
    consume(Lexeme.LEFT_PARENT, MissingArguments("Missing list of arguments at `defn`, found: " + currentToken))
    var args: List[String] = List.empty
    while (currentLexeme != Lexeme.RIGHT_PARENT) {
      if (!matchLexeme(Lexeme.IDENT))
        throw new IllegalArgumentException("Unknown token in list of function arguments: " + currentToken)
      args = args :+ currentToken.value.asInstanceOf[String]
      nextToken()
    }
    nextToken()
    val body = parseExpression()
    nextToken()
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `defn` expression, found: " + currentToken)
    new DefnExpression(function, args, body)
  }


  private def parseLet(): LetExpression = {
    nextToken()
    val identifier = currentToken
    consume(
      Lexeme.IDENT,
      MissingSymbolException("Expected identifier after `let`, found: " + identifier))
    val assigned = parseExpression()
    nextToken()
    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `let` expression, found: " + currentToken)

    new LetExpression(identifier, assigned)
  }

  private def parseDo(): DoExpression = {
    nextToken()
    var expressions: List[Expression] = List.empty
    while (currentLexeme != Lexeme.RIGHT_PARENT) {
      expressions = expressions :+ parseExpression()
      nextToken()
    }

    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `do` expression, found: " + currentToken)

    new DoExpression(expressions)
  }

  private def parseCompose(): ComposeExpression = {
    nextToken()
    val first = parseExpression()
    var functionCalls: List[FnCallExpression] = List.empty
    nextToken()
    while (currentLexeme != Lexeme.RIGHT_PARENT) {
      val expr = parseExpression()
      if (!expr.isInstanceOf[FnCallExpression])
        throw MissingArguments("Missing function call" + currentToken)

      functionCalls = functionCalls :+ expr.asInstanceOf[FnCallExpression]
      nextToken()
    }

    if (!matchLexeme(Lexeme.RIGHT_PARENT))
      throw MissingRightParent("Missing parenthesis at the end of `->` expression, found: " + currentToken)

    new ComposeExpression(first, functionCalls)
  }



  /* ******************************************************************************
   ********************************* PARSER LOGIC *********************************
   ****************************************************************************** */

  nextToken()
  while (!matchLexeme(Lexeme.EOF)) {
    programBody = programBody :+ parseExpression()
    nextToken()
  }
}
