package haruko.compiler;

public interface Visitor {
    void visitFnCall(FnCallExpression e, Environment env);
    void visitDef(DefExpression e, Environment env);
    void visitLet(LetExpression e, Environment env);
    void visitDefn(DefnExpression e, Environment env);
    void visitIf(IfExpression e, Environment env);
    void visitDo(DoExpression e, Environment env);
    void visitConst(ConstExpression e, Environment env);
    void visitSymbol(SymExpression e, Environment env);
}