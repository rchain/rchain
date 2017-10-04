package rholang.parsing.delimc;
import rholang.parsing.delimc.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* TypedExpr */
    public R visit(rholang.parsing.delimc.Absyn.ETyped p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.delimc.Absyn.TypedExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Expr */
    public R visit(rholang.parsing.delimc.Absyn.EVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EVal p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EAbs p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EApp p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EReturn p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EBind p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.ENewPrompt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EPushPrompt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EWithSubCont p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.EPushSubCont p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.ETuple p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.delimc.Absyn.Expr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Tuple */
    public R visit(rholang.parsing.delimc.Absyn.Tuple2 p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.Tuple3 p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.delimc.Absyn.Tuple p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(rholang.parsing.delimc.Absyn.VInt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.VString p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.delimc.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Type */
    public R visit(rholang.parsing.delimc.Absyn.TSimple p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.delimc.Absyn.TTuple p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.delimc.Absyn.TMonad p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.TFun p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.delimc.Absyn.Type p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TType */
    public R visit(rholang.parsing.delimc.Absyn.TType2 p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.delimc.Absyn.TType3 p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.delimc.Absyn.TType p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
