package rholang.parsing.lambda;

import rholang.parsing.lambda.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* TypedExpr */
    public R visit(rholang.parsing.lambda.Absyn.ETyped p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }

/* Expr */
    public R visit(rholang.parsing.lambda.Absyn.EVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.EVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.EAbs p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.typedexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.EApp p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_1.accept(this, arg), r, arg);
      r = combine(p.typedexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.ETuple p, A arg) {
      R r = leaf(arg);
      r = combine(p.tuple_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.EFirst p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.ESecond p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.EThird p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_.accept(this, arg), r, arg);
      return r;
    }

/* Tuple */
    public R visit(rholang.parsing.lambda.Absyn.Tuple2 p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_1.accept(this, arg), r, arg);
      r = combine(p.typedexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.Tuple3 p, A arg) {
      R r = leaf(arg);
      r = combine(p.typedexpr_1.accept(this, arg), r, arg);
      r = combine(p.typedexpr_2.accept(this, arg), r, arg);
      r = combine(p.typedexpr_3.accept(this, arg), r, arg);
      return r;
    }

/* Value */
    public R visit(rholang.parsing.lambda.Absyn.VInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.VString p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Type */
    public R visit(rholang.parsing.lambda.Absyn.TSimple p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.TTuple p, A arg) {
      R r = leaf(arg);
      r = combine(p.ttype_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.TFun p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_1.accept(this, arg), r, arg);
      r = combine(p.type_2.accept(this, arg), r, arg);
      return r;
    }

/* TType */
    public R visit(rholang.parsing.lambda.Absyn.TType2 p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_1.accept(this, arg), r, arg);
      r = combine(p.type_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.lambda.Absyn.TType3 p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_1.accept(this, arg), r, arg);
      r = combine(p.type_2.accept(this, arg), r, arg);
      r = combine(p.type_3.accept(this, arg), r, arg);
      return r;
    }


}
