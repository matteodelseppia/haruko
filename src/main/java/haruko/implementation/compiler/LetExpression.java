package haruko.implementation.compiler;

public class LetExpression extends Expression {
    final String variableName;
    final Expression binding;
    final Expression body;

    public LetExpression(String variableName, Expression binding, Expression body) {
        this.variableName = variableName;
        this.binding = binding;
        this.body = body;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitLet(this);
    }
}
