package haruko.implementation.compiler;

import scala.collection.immutable.List;

public class CondExpression extends Expression {
    final List<Expression> tests;

    public CondExpression(List<Expression> tests) {
        this.tests = tests;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitCond(this);
    }
}
