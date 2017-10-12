package rholang.parsing.lambda;
import rholang.parsing.lambda.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  rholang.parsing.lambda.Absyn.TypedExpr.Visitor<rholang.parsing.lambda.Absyn.TypedExpr,A>,
  rholang.parsing.lambda.Absyn.Expr.Visitor<rholang.parsing.lambda.Absyn.Expr,A>,
  rholang.parsing.lambda.Absyn.Tuple.Visitor<rholang.parsing.lambda.Absyn.Tuple,A>,
  rholang.parsing.lambda.Absyn.Value.Visitor<rholang.parsing.lambda.Absyn.Value,A>,
  rholang.parsing.lambda.Absyn.Type.Visitor<rholang.parsing.lambda.Absyn.Type,A>,
  rholang.parsing.lambda.Absyn.TType.Visitor<rholang.parsing.lambda.Absyn.TType,A>
{
/* TypedExpr */
    public TypedExpr visit(rholang.parsing.lambda.Absyn.ETyped p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      Type type_ = p.type_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.ETyped(expr_, type_);
    }
/* Expr */
    public Expr visit(rholang.parsing.lambda.Absyn.EVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.lambda.Absyn.EVar(var_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.EVal p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.EVal(value_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.EAbs p, A arg)
    {
      String var_ = p.var_;
      Type type_ = p.type_.accept(this, arg);
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.EAbs(var_, type_, typedexpr_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.EApp p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.EApp(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.lambda.Absyn.ETuple p, A arg)
    {
      Tuple tuple_ = p.tuple_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.ETuple(tuple_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.EFirst p, A arg)
    {
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.EFirst(typedexpr_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.ESecond p, A arg)
    {
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.ESecond(typedexpr_);
    }    public Expr visit(rholang.parsing.lambda.Absyn.EThird p, A arg)
    {
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.EThird(typedexpr_);
    }
/* Tuple */
    public Tuple visit(rholang.parsing.lambda.Absyn.Tuple2 p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.Tuple2(typedexpr_1, typedexpr_2);
    }    public Tuple visit(rholang.parsing.lambda.Absyn.Tuple3 p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      TypedExpr typedexpr_3 = p.typedexpr_3.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.Tuple3(typedexpr_1, typedexpr_2, typedexpr_3);
    }
/* Value */
    public Value visit(rholang.parsing.lambda.Absyn.VInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new rholang.parsing.lambda.Absyn.VInt(integer_);
    }    public Value visit(rholang.parsing.lambda.Absyn.VString p, A arg)
    {
      String string_ = p.string_;
      return new rholang.parsing.lambda.Absyn.VString(string_);
    }
/* Type */
    public Type visit(rholang.parsing.lambda.Absyn.TSimple p, A arg)
    {
      String simpletype_ = p.simpletype_;
      return new rholang.parsing.lambda.Absyn.TSimple(simpletype_);
    }    public Type visit(rholang.parsing.lambda.Absyn.TTuple p, A arg)
    {
      TType ttype_ = p.ttype_.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.TTuple(ttype_);
    }    public Type visit(rholang.parsing.lambda.Absyn.TFun p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.TFun(type_1, type_2);
    }
/* TType */
    public TType visit(rholang.parsing.lambda.Absyn.TType2 p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.TType2(type_1, type_2);
    }    public TType visit(rholang.parsing.lambda.Absyn.TType3 p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      Type type_3 = p.type_3.accept(this, arg);
      return new rholang.parsing.lambda.Absyn.TType3(type_1, type_2, type_3);
    }
}