package haruko.implementation.compiler;

public interface Visitor {
    void visitFnCall(FnCallExpression e);
    void visitDef(DefExpression e);
    void visitLet(LetExpression e);
    void visitDefn(DefnExpression e);
    void visitIf(IfExpression e);
    void visitDo(DoExpression e);
    void visitCond(CondExpression e);
    void visitConst(ConstExpression e);
    void visitSymbol(SymExpression e);
}