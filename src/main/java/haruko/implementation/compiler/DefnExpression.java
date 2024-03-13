package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;
import scala.collection.immutable.List;

public class DefnExpression extends Expression {
    final Token functionName;
    final List<String> arguments;
    final Expression body;

    public DefnExpression(Token functionName, List<String> arguments, Expression body) {
        this.functionName = functionName;
        this.arguments = arguments;
        this.body = body;
    }

    @Override
    void accept(Visitor visitor) {
        visitor.visitDefn(this);
    }

    @Override
    public String toString() {
        return "DefnExpression{" +
                "functionName=" + functionName +
                ", arguments=" + arguments +
                ", body=" + body +
                '}';
    }
}
