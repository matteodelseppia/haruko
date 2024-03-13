package haruko.compiler;

import scala.collection.immutable.List;

public class ComposeExpression extends Expression {
    Expression first;
    List<FnCallExpression> functionCalls;

    public ComposeExpression(Expression first, List<FnCallExpression> functionCalls) {
        this.first = first;
        this.functionCalls = functionCalls;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitCompose(this, env);
    }
}
