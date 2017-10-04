package rholang.parsing.lambda;
import rholang.parsing.lambda.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* TypedExpr */
    public R visit(rholang.parsing.lambda.Absyn.ETyped p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.lambda.Absyn.TypedExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Expr */
    public R visit(rholang.parsing.lambda.Absyn.EVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.EVal p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.EAbs p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.EApp p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.ETuple p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.EFirst p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.ESecond p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.EThird p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.lambda.Absyn.Expr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Tuple */
    public R visit(rholang.parsing.lambda.Absyn.Tuple2 p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.Tuple3 p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.lambda.Absyn.Tuple p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(rholang.parsing.lambda.Absyn.VInt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.VString p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.lambda.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Type */
    public R visit(rholang.parsing.lambda.Absyn.TSimple p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.lambda.Absyn.TTuple p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.lambda.Absyn.TFun p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.lambda.Absyn.Type p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* TType */
    public R visit(rholang.parsing.lambda.Absyn.TType2 p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.lambda.Absyn.TType3 p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.lambda.Absyn.TType p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
