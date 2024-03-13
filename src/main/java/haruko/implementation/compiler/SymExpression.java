package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class SymExpression extends Expression {
    final Token symbol;

    public SymExpression(Token symbol) {
        this.symbol = symbol;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitSymbol(this);
    }

    @Override
    public String toString() {
        return "SymExpression{" +
                "symbol=" + symbol +
                '}';
    }
}
