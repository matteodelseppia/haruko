package haruko.implementation.lexer;

public class Token {
    public final Lexeme lexeme;
    public final Object value;
    public final int column;
    public final int line;

    public Token(Lexeme lexeme, Object value, int line, int column) {
        this.lexeme = lexeme;
        this.value = value;
        this.column = column;
        this.line = line;
    }

    public Token(Lexeme lexeme, int line, int column) {
        this.lexeme = lexeme;
        this.value = null;
        this.column = column;
        this.line = line;
    }

    @Override
    public String toString() {
        return "Token{" +
                "lexeme=" + lexeme +
                ", value=" + value +
                ", column=" + column +
                ", line=" + line +
                '}';
    }
}