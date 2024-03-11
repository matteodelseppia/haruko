package haruko.implementation.compiler;

import scala.collection.immutable.List;

public class DoExpression extends Expression {
    final List<Expression> expressions;

    public DoExpression(List<Expression> expressions) {
        this.expressions = expressions;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitDo(this);
    }
}
