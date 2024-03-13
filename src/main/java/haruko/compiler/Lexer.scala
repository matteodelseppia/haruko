package haruko.compiler

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

/**
 * Tokens are pairs of a {@link Lexeme} and possibly an {@link Object}, encountered
 * while scanning the source code. They are the output of the {@link Lexer}. They
 * also hold information about the line and column where they were encountered.
 * @param lexeme the Lexeme associated to the Token.
 * @param value the Object associated to the Token (if any).
 * @param line the source line where the Token was encountered.
 * @param column the source column where the Token was encountered.
 */
case class Token(lexeme: Lexeme, value: Object, line: Int, column: Int)


/**
 * The generic exception thrown when the {@link Lexer} encounters a syntactical
 * error in the source code.
 * @param msg The message to show to the user.
 */
private abstract class LexerException(msg: String) extends Exception(msg)


/**
 * Exception thrown when the end of file is reached before a string terminating ' " '
 * character is encountered.
 * @param msg The message to show to the user.
 */
private case class UnterminatedStringException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when a forbidden character is encountered while scanning the source.
 * @param msg The message to show to the user.
 */
private case class ForbiddenCharException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when the source file is found empty.
 * @param msg The message to show to the user.
 */
private case class EmptySourceException(msg: String) extends LexerException(msg)


/**
 * Exception thrown when an invalid numeric {@link Token} is encountered while
 * scanning the source.
 * @param msg The message to show to the user.
 */
private case class InvalidNumberException(msg: String) extends LexerException(msg)


/**
 * The Lexer scans the source one character at a time, producing
 * an {@link ArrayBuffer} of {@link Token} that will serve as input for
 * the {@link Parser}.
 * @param source a {@link String} containing the program source.
 */
class Lexer(val source: String) {

  if (source.isEmpty) throw EmptySourceException("Error: source is empty")

  private val tokens = new ArrayBuffer[Token]
  private val integer_pattern = Pattern.compile("[+-]?(?!0\\d)\\d+$")
  private val double_pattern = Pattern.compile("([-+]?(?!0\\d)\\d+(\\.\\d*)?([eE][-+]?\\d+)?)")

  private var token_start = 0
  private var token_current = 0
  private var current_line = 1
  private var current_column = 0
  private var current_char = source.charAt(0)

  def getTokens: ArrayBuffer[Token] = {
    tokens.addOne(new Token(Lexeme.EOF, null, current_line, current_column))
  }

  private def nextChar(): Unit = {
    token_current += 1
    current_column += 1

    if (token_current >= source.length)
      current_char = 0
    else
      current_char = source.charAt(token_current)

    if (current_char == '\n') {
      current_line += 1
      current_column = 0
    }
  }

  private def undoNext(): Unit = {
    token_current -= 1
    current_char = source.charAt(token_current)
    if (current_char == '\n') current_line -= 1
  }

  private object NumberReader {
    private def isTerminalChar(c: Char): Boolean = {
      c match {
        case c if c.isWhitespace => true
        case ')' | '(' | '[' | ']' | '{' | '}' => true
        case _ => false
      }
    }

    def read(): Unit = {
      while (token_current < source.length && !isTerminalChar(current_char)) nextChar()

      val number_string = source.substring(token_start, token_current)

      if (integer_pattern.matcher(number_string).matches())
        tokens.addOne(new Token(Lexeme.CONST_L, number_string.toLong.asInstanceOf[Object], current_line, current_column))

      else if (double_pattern.matcher(number_string).matches())
        tokens.addOne(new Token(Lexeme.CONST_D, number_string.toDouble.asInstanceOf[Object], current_line, current_column))

      else
        throw InvalidNumberException("Error: invalid number")
    }
  }

  private object StringReader {
    def read(): Unit = {
      nextChar()
      while (token_current < source.length && current_char != '\"') nextChar()

      if (current_char != '\"')
        throw UnterminatedStringException("Error: string unterminated before end of file")

      nextChar()
      val string = source.substring(token_start + 1, token_current - 1)
      tokens.addOne(new Token(Lexeme.CONST_S, string, current_line, current_column))
    }
  }

  private object WordReader {
    private def isTerminalChar(c: Char): Boolean = {
      c match {
        case c if c.isLetterOrDigit => false
        case '\'' | '*' | '+' | '-' | '>' | '<' | '!' | '^' | '/' | '=' | '$' => false
        case _ => true
      }
    }

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

    def read(): Unit = {
      while (token_current < source.length && !isTerminalChar(current_char)) nextChar()

      val lex_string = source.substring(token_start, token_current)
      val lexeme = whichLexeme(lex_string)

      lexeme match {
        case Lexeme.IDENT => tokens.addOne(new Token(lexeme, lex_string, current_line, current_column))
        case Lexeme.TRUE => tokens.addOne(new Token(lexeme, true.asInstanceOf[Object], current_line, current_column))
        case Lexeme.FALSE => tokens.addOne(new Token(lexeme, false.asInstanceOf[Object], current_line, current_column))
        case _ => tokens.addOne(new Token(lexeme, null, current_line, current_column))
      }
    }
  }


  while (token_current < source.length) {
    token_start = token_current

    current_char match {
      case c if c.isWhitespace => nextChar()
      case c if c.isDigit => NumberReader.read()
      case '^' => WordReader.read()
      case '\"' => StringReader.read()

      case '+' =>
        nextChar()
        if (current_char.isDigit) {
          undoNext()
          NumberReader.read()
        } else {
          undoNext()
          WordReader.read()
        }
        
      case '-' =>
        nextChar()
        if (current_char.isDigit) {
          undoNext()
          NumberReader.read()
        } else {
          undoNext()
          WordReader.read()
        }

      case '(' =>
        tokens.addOne(new Token(Lexeme.LEFT_PARENT, null, current_line, current_column))
        nextChar()

      case ')' =>
        tokens.addOne(new Token(Lexeme.RIGHT_PARENT, null, current_line, current_column))
        nextChar()

      case '[' =>
        tokens.addOne(new Token(Lexeme.LEFT_BRACKET, null, current_line, current_column))
        nextChar()

      case ']' =>
        tokens.addOne(new Token(Lexeme.RIGHT_BRACKET, null, current_line, current_column))
        nextChar()

      case '{' =>
        tokens.addOne(new Token(Lexeme.LEFT_BRACE, null, current_line, current_column))
        nextChar()

      case '}' =>
        tokens.addOne(new Token(Lexeme.RIGHT_BRACE, null, current_line, current_column))
        nextChar()

      case _ => WordReader.read()
    }
  }
}
