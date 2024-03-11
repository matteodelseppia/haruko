package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class SymExpression extends Expression {
    final Token symbol;

    public SymExpression(Token symbol) {
        this.symbol = symbol;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitSymbol(this);
    }
}
