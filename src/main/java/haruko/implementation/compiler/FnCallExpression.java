package haruko.implementation.compiler;

import haruko.implementation.lexer.Lexeme;
import scala.collection.immutable.List;

public class FnCallExpression extends Expression {
    final Lexeme type;
    final String functionName;
    final List<Expression> arguments;

    public FnCallExpression(Lexeme type, String functionName, List<Expression> arguments) {
        this.type = type;
        this.functionName = functionName;
        this.arguments = arguments;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitFnCall(this);
    }
}
