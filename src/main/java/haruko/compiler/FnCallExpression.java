package haruko.compiler;

import scala.collection.immutable.List;

public class FnCallExpression extends Expression {
    final Token functionName;
    final List<Expression> arguments;

    public FnCallExpression(Token functionName, List<Expression> arguments) {
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitFnCall(this, env);
    }

    @Override
    public String toString() {
        return "FnCallExpression{" +
                "functionName=" + functionName +
                ", arguments=" + arguments +
                '}';
    }
}
