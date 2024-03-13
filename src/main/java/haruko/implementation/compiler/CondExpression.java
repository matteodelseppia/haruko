package haruko.implementation.compiler;

import scala.collection.immutable.List;

public class CondExpression extends Expression {
    final List<Expression> tests;

    public CondExpression(List<Expression> tests) {
        this.tests = tests;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitCond(this);
    }

    @Override
    public String toString() {
        return "CondExpression{" +
                "tests=" + tests +
                '}';
    }
}
