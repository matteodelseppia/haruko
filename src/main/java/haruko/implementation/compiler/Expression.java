package haruko.implementation.compiler;

public abstract class Expression {
    abstract <R> void accept(Visitor<R> visitor);
}
