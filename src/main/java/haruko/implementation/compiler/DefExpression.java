package haruko.implementation.compiler;

import haruko.implementation.lexer.Token;

public class DefExpression extends Expression {
    final Token variableName;
    final Expression assignedValue;

    public DefExpression(Token variable, Expression assignedValue) {
        this.variableName = variable;
        this.assignedValue = assignedValue;
    }

    @Override
    <R> void accept(Visitor<R> visitor) {
        visitor.visitDef(this);
    }
}
