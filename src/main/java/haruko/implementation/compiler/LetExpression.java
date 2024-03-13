package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class LetExpression extends Expression {
    final Token variableName;
    final Expression binding;
    final Expression body;

    public LetExpression(Token variableName, Expression binding, Expression body) {
        this.variableName = variableName;
        this.binding = binding;
        this.body = body;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitLet(this);
    }

    @Override
    public String toString() {
        return "LetExpression{" +
                "variableName=" + variableName +
                ", binding=" + binding +
                ", body=" + body +
                '}';
    }
}
