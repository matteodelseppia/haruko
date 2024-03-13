package haruko.compiler;

public class SymExpression extends Expression {
    final Token symbol;

    public SymExpression(Token symbol) {
        this.symbol = symbol;
    }

    @Override
    void accept(Visitor visitor, Environment env) {
        visitor.visitSymbol(this, env);
    }
    
    @Override
    public String toString() {
        return "SymExpression{" +
                "symbol=" + symbol +
                '}';
    }
}
