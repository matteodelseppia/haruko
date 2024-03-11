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
    <R> void accept(Visitor<R> visitor) {
        visitor.visitIf(this);
    }
}
