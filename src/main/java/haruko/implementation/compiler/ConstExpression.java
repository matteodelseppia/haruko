package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class ConstExpression extends Expression {
    final Token constant;

    public ConstExpression(Token token) {
        this.constant = token;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitConst(this, env);
    }

    @Override
    public String toString() {
        return "ConstExpression{" +
                "constant=" + constant +
                '}';
    }
}
