package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class LetExpression extends Expression {
    final Token variableName;
    final Expression binding;

    public LetExpression(Token variableName, Expression binding) {
        this.variableName = variableName;
        this.binding = binding;
    }

    @Override
    public String toString() {
        return "LetExpression{" +
                "variableName=" + variableName +
                ", binding=" + binding +
                '}';
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitLet(this, env);
    }
}
