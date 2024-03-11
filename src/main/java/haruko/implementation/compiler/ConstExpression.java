package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class ConstExpression extends Expression {
    final Token constant;

    public ConstExpression(Token token) {
        this.constant = token;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitConst(this);
    }
}
