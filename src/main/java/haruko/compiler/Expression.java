package haruko.compiler;

public abstract class Expression {
    abstract void accept(Visitor visitor, Environment env);
}
