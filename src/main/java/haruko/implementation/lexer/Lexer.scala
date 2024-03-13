package haruko.implementation.lexer

import haruko.implementation.lexer.LexerExceptions.{ForbiddenCharException, InvalidNumberException, UnterminatedStringException}

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

object LexerExceptions {
  case class UnterminatedStringException(msg: String) extends Exception(msg)
  case class ForbiddenCharException(msg: String) extends Exception(msg)
  case class EmptySourceException(msg: String) extends Exception(msg)
  case class InvalidNumberException(msg: String) extends NumberFormatException(msg)
}

class Lexer(val source: String) {

  if (source.isEmpty) throw LexerExceptions.EmptySourceException("Error: source is empty")

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
        tokens.addOne(new Token(Lexeme.CONST_L, number_string.toLong, current_line, current_column))

      else if (double_pattern.matcher(number_string).matches())
        tokens.addOne(new Token(Lexeme.CONST_D, number_string.toDouble, current_line, current_column))

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
        case Lexeme.TRUE => tokens.addOne(new Token(lexeme, true, current_line, current_column))
        case Lexeme.FALSE => tokens.addOne(new Token(lexeme, false, current_line, current_column))
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
