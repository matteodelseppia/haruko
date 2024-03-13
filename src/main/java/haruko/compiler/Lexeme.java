package haruko.compiler;

public enum Lexeme {
  LEFT_PARENT,
  RIGHT_PARENT,
  LEFT_BRACKET,
  RIGHT_BRACKET,
  LEFT_BRACE, 
  COND,
  LET,
  DO,
  RIGHT_BRACE,
  ADD,
  SUB,
  MUL,
  DIV,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  BANG,
  NOT_EQUAL,
  EQUAL,
  IDENT,
  DEF,
  UNKNOWN,
  TRUE,
  FALSE,
  IF,
  AND,
  OR,
  NOT,
  CONST_L,
  CONST_D,
  CONST_S,
  PRLN,
  NIL,
  DEFN,
  EOF;
}