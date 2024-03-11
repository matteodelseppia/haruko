package haruko.implementation.compiler;

import scala.collection.immutable.List;

public class DefnExpression extends Expression {
    final String functionName;
    final List<String> arguments;
    final List<Expression> body;

    public DefnExpression(String functionName, List<String> arguments, List<Expression> body) {
        this.functionName = functionName;
        this.arguments = arguments;
        this.body = body;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitDefn(this);
    }
}
