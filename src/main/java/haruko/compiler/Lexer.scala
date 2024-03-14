package haruko.compiler

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

/**
 * Tokens are pairs of a {@link Lexeme} and possibly an {@link Object}, encountered
 * while scanning the source code. They are the output of the {@link Lexer}. They
 * also hold information about the line and column where they were encountered.
 *
 * @param lexeme the Lexeme associated to the Token.
 * @param value  the Object associated to the Token (if any).
 * @param line   the source line where the Token was encountered.
 * @param column the source column where the Token was encountered.
 */
case class Token(lexeme: Lexeme, value: Object, line: Int, column: Int)


/**
 * The generic exception thrown when the {@link Lexer} encounters a syntactical
 * error in the source code.
 *
 * @param msg The message to show to the user.
 */
private abstract class LexerException(msg: String) extends Exception(msg)


/**
 * Exception thrown when the end of file is reached before a string terminating ' " '
 * character is encountered.
 *
 * @param msg The message to show to the user.
 */
private case class UnterminatedStringException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when a forbidden character is encountered while scanning the source.
 *
 * @param msg The message to show to the user.
 */
private case class ForbiddenCharException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when the source file is found empty.
 *
 * @param msg The message to show to the user.
 */
private case class EmptySourceException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when an invalid numeric {@link Token} is encountered while
 * scanning the source.
 *
 * @param msg The message to show to the user.
 */
private case class InvalidNumberException(msg: String) extends LexerException(msg)


/**
 * The Lexer scans the source one character at a time, producing
 * an <code>ArrayBuffer</code> of {@link Token} that will serve as input for
 * the {@link Parser}.
 *
 * @param source a String containing the program source.
 */
class Lexer(val source: String) {

  /**
   * The <code>tokens</code> encountered while scanning the source.
   */
  private val tokens = new ArrayBuffer[Token]

  /**
   * A counter that tracks the beginning position of the current token in the source.
   */
  private var tokenStart = 0

  /**
   * A counter that tracks the position of the current character
   * in the source.
   */
  private var tokenCurrent = 0

  /**
   * A counter that tracks the current line.
   */
  private var currentLine = 1

  /**
   * A counter that tracks the current column.
   */
  private var currentColumn = 0

  /**
   * The character at position <code>tokenCurrent</code>.
   */
  private var currentChar = source.charAt(0)


  /**
   * A simple getter that can be used to retrieve the tokens found in the source, with the
   * <code>EOF Lexeme</code> as last token.
   *
   * @return the <code>ArrayBuffer</code> holding the tokens found in the source.
   */
  def getTokens: ArrayBuffer[Token] = {
    tokens.addOne(Token(Lexeme.EOF, null, currentLine, currentColumn))
  }

  /**
   * Increases by one <code>tokenCurrent</code>, updating the other trackers as well.
   * <code>tokenStart</code> does not change.
   */
  private def nextChar(): Unit = {
    tokenCurrent += 1
    currentColumn += 1

    if (tokenCurrent >= source.length)
      currentChar = 0
    else
      currentChar = source.charAt(tokenCurrent)

    if (currentChar == '\n') {
      currentLine += 1
      currentColumn = 0
    }
  }

  /**
   * Restores the tracker variables to the state they had before the last call to {@link nextChar}.
   */
  private def undoNext(): Unit = {
    tokenCurrent -= 1
    currentChar = source.charAt(tokenCurrent)
    if (currentChar == '\n')
      currentLine -= 1
  }

  /**
   * The <code>NumberReader</code> is invoked by the Lexer when a +/- and/or a digit is encountered.
   * Its job is reading one number and adding it to the <code>tokens</code> buffer. It can throw
   * a {@link InvalidNumberException} if there are problems with the number formatting.
   */
  private object NumberReader {

    /**
     * The pattern to match integral values, allowing a leading plus or minus.
     */
    private val integerPattern = Pattern.compile("[+-]?(?!0\\d)\\d+$")

    /**
     * The pattern to match floating point decimal numbers, allowing a leading
     * plus or minus and scientific notation, e.g. <code>+2e-3</code>.
     */
    private val doublePattern = Pattern.compile("([-+]?(?!0\\d)\\d+(\\.\\d*)?([eE][-+]?\\d+)?)")


    /**
     * Checks if the current character can be considered the end of the number being read.
     * Parentheses and whitespace are considered the end of numeric constants. Expressions
     * like <code> (add 2x) </code> where <code>x</code> is a variable are hence forbidden.
     *
     * @param c the character to check
     * @return <code>true</code> if we are at the end of the numeric constant, <code>false</code> otherwise.
     */
    private def isTerminalChar(c: Char): Boolean = {
      c match {
        case c if c.isWhitespace => true
        case ')' | '(' | '[' | ']' | '{' | '}' => true
        case _ => false
      }
    }

    /**
     * Reads one number and adds it to the <code>tokens</code> buffer. If the number does not
     * match the pattern for neither integer or decimal values, an {@link InvalidNumberException} is thrown.
     */
    def read(): Unit = {
      while (tokenCurrent < source.length && !isTerminalChar(currentChar)) nextChar()

      val numberString = source.substring(tokenStart, tokenCurrent)

      if (integerPattern.matcher(numberString).matches())
        tokens.addOne(Token(Lexeme.CONST_L, numberString.toLong.asInstanceOf[Object], currentLine, currentColumn))

      else if (doublePattern.matcher(numberString).matches())
        tokens.addOne(Token(Lexeme.CONST_D, numberString.toDouble.asInstanceOf[Object], currentLine, currentColumn))

      else
        throw InvalidNumberException("Error: invalid number " + numberString)
    }
  }

  /**
   * The <code>StringReader</code> reads one string constant from the source and adds it to the <code>tokens</code>
   * buffer. It is invoked by the Lexer when a double quote character is found in the source.
   */
  private object StringReader {

    /**
     * Reads all characters until another double quote character is found. This allows strings to be multiline.
     * If the file terminates before a double quote character is encountered, an {@link UnterminatedStringException}
     * is thrown.
     */
    def read(): Unit = {
      nextChar()
      while (tokenCurrent < source.length && currentChar != '\"') nextChar()

      if (currentChar != '\"')
        throw UnterminatedStringException("Error: string unterminated before end of file")

      nextChar()
      val string = source.substring(tokenStart + 1, tokenCurrent - 1)
      tokens.addOne(Token(Lexeme.CONST_S, string, currentLine, currentColumn))
    }
  }

  /**
   * The <code>WordReader</code> reads a word from the source code and adds it to the
   * <code>tokens</code> buffer. Words can either be reserved keywords like
   * <code>let,if,defn</code> or identifiers.
   */
  private object WordReader {

    /**
     * Checks if a character can be considered the end of the word. Words can start with only
     * alphabetic characters but can contain characters commonly considered reserved.
     *
     * @param c the character to check.
     * @return <code>true</code> if c is not considered part of the current word, <code>false</code> otherwise.
     */
    private def isTerminalChar(c: Char): Boolean = {
      c match {
        case c if c.isLetterOrDigit => false
        case '\'' | '*' | '+' | '-' | '>' | '<' | '!' | '^' | '/' | '=' | '$' => false
        case _ => true
      }
    }

    /**
     * Matches a string with the reserved keywords. If the string does not match any reserved
     * keyword, it is considered an identifier.
     *
     * @param s the string to match.
     * @return the Lexeme associated to the word.
     */
    private def whichLexeme(s: String): Lexeme = {
      s match {
        case "def" => Lexeme.DEF
        case "defn" => Lexeme.DEFN
        case "nil" => Lexeme.NIL
        case "true" => Lexeme.TRUE
        case "false" => Lexeme.FALSE
        case "let" => Lexeme.LET
        case "if" => Lexeme.IF
        case "cond" => Lexeme.COND
        case "do" => Lexeme.DO
        case _ => Lexeme.IDENT
      }
    }

    /**
     * Reads one word and adds it to the <code>tokens</code> buffer.
     */
    def read(): Unit = {
      while (tokenCurrent < source.length && !isTerminalChar(currentChar)) nextChar()

      val lexString = source.substring(tokenStart, tokenCurrent)
      val lexeme = whichLexeme(lexString)

      lexeme match {
        case Lexeme.IDENT => tokens.addOne(Token(lexeme, lexString, currentLine, currentColumn))
        case Lexeme.TRUE => tokens.addOne(Token(lexeme, true.asInstanceOf[Object], currentLine, currentColumn))
        case Lexeme.FALSE => tokens.addOne(Token(lexeme, false.asInstanceOf[Object], currentLine, currentColumn))
        case _ => tokens.addOne(Token(lexeme, null, currentLine, currentColumn))
      }
    }
  }


  /* ******************************************************************************
     ********************************* LEXER LOGIC ********************************
     ****************************************************************************** */

  if (source.isEmpty) throw EmptySourceException("Lexer error: source is empty")

  while (tokenCurrent < source.length) {
    tokenStart = tokenCurrent

    currentChar match {
      case c if c.isWhitespace => nextChar()
      case c if c.isDigit => NumberReader.read()
      case '\"' => StringReader.read()

      case '+' =>
        nextChar()
        if (currentChar.isDigit) {
          undoNext()
          NumberReader.read()
        } else {
          undoNext()
          WordReader.read()
        }

      case '-' =>
        nextChar()
        if (currentChar.isDigit) {
          undoNext()
          NumberReader.read()
        } else if (currentChar == '>') {
          tokens.addOne(Token(Lexeme.COMPOSE, null, currentLine, currentColumn))
          nextChar()
        } else {
          undoNext()
          WordReader.read()
        }

      case '(' =>
        tokens.addOne(Token(Lexeme.LEFT_PARENT, null, currentLine, currentColumn))
        nextChar()

      case ')' =>
        tokens.addOne(Token(Lexeme.RIGHT_PARENT, null, currentLine, currentColumn))
        nextChar()

      case '[' =>
        tokens.addOne(Token(Lexeme.LEFT_BRACKET, null, currentLine, currentColumn))
        nextChar()

      case ']' =>
        tokens.addOne(Token(Lexeme.RIGHT_BRACKET, null, currentLine, currentColumn))
        nextChar()

      case '{' =>
        tokens.addOne(Token(Lexeme.LEFT_BRACE, null, currentLine, currentColumn))
        nextChar()

      case '}' =>
        tokens.addOne(Token(Lexeme.RIGHT_BRACE, null, currentLine, currentColumn))
        nextChar()

      case '_' =>
        tokens.addOne(Token(Lexeme.SKIP, null, currentLine, currentColumn))
        nextChar()

      case _ => WordReader.read()
    }
  }
}
