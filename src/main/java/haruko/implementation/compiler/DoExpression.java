package haruko.implementation.compiler;

import scala.collection.immutable.List;

public class DoExpression extends Expression {
    final List<Expression> expressions;

    public DoExpression(List<Expression> expressions) {
        this.expressions = expressions;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitDo(this);
    }

    @Override
    public String toString() {
        return "DoExpression{" +
                "expressions=" + expressions +
                '}';
    }
}
