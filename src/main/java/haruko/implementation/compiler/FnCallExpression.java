package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;
import scala.collection.immutable.List;

public class FnCallExpression extends Expression {
    final Token functionName;
    final List<Expression> arguments;

    public FnCallExpression(Token functionName, List<Expression> arguments) {
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitFnCall(this);
    }

    @Override
    public String toString() {
        return "FnCallExpression{" +
                "functionName=" + functionName +
                ", arguments=" + arguments +
                '}';
    }
}
