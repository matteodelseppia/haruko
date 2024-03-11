package haruko.implementation.lexer;

public record Token(Lexeme lexeme, 
                    Object value, 
                    Integer line, 
                    Integer column) {}