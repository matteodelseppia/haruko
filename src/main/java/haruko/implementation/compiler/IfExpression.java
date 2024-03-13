package haruko.implementation.compiler;

public class IfExpression extends Expression {
    final Expression condition;
    final Expression ifTrue;
    final Expression ifFalse;

    public IfExpression(Expression condition, Expression ifTrue, Expression ifFalse) {
        this.condition = condition;
        this.ifTrue = ifTrue;
        this.ifFalse = ifFalse;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitIf(this, env);
    }

    @Override
    public String toString() {
        return "IfExpression{" +
                "condition=" + condition +
                ", ifTrue=" + ifTrue +
                ", ifFalse=" + ifFalse +
                '}';
    }
}
