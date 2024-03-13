package haruko.compiler;

public class DefExpression extends Expression {
    public final Token variableName;
    public final Expression assignedValue;

    public DefExpression(Token variable, Expression assignedValue) {
        this.variableName = variable;
        this.assignedValue = assignedValue;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitDef(this, env);
    }
    
    @Override
    public String toString() {
        return "DefExpression{" +
                "variableName=" + variableName +
                ", assignedValue=" + assignedValue +
                '}';
    }
}
