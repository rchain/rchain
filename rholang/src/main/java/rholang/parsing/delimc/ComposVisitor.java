package rholang.parsing.delimc;
import rholang.parsing.delimc.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  rholang.parsing.delimc.Absyn.TypedExpr.Visitor<rholang.parsing.delimc.Absyn.TypedExpr,A>,
  rholang.parsing.delimc.Absyn.Expr.Visitor<rholang.parsing.delimc.Absyn.Expr,A>,
  rholang.parsing.delimc.Absyn.Tuple.Visitor<rholang.parsing.delimc.Absyn.Tuple,A>,
  rholang.parsing.delimc.Absyn.Value.Visitor<rholang.parsing.delimc.Absyn.Value,A>,
  rholang.parsing.delimc.Absyn.Type.Visitor<rholang.parsing.delimc.Absyn.Type,A>,
  rholang.parsing.delimc.Absyn.TType.Visitor<rholang.parsing.delimc.Absyn.TType,A>
{
/* TypedExpr */
    public TypedExpr visit(rholang.parsing.delimc.Absyn.ETyped p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      Type type_ = p.type_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.ETyped(expr_, type_);
    }
/* Expr */
    public Expr visit(rholang.parsing.delimc.Absyn.EVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.delimc.Absyn.EVar(var_);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EVal p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EVal(value_);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EAbs p, A arg)
    {
      String var_ = p.var_;
      Type type_ = p.type_.accept(this, arg);
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EAbs(var_, type_, typedexpr_);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EApp p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EApp(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EReturn p, A arg)
    {
      TypedExpr typedexpr_ = p.typedexpr_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EReturn(typedexpr_);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EBind p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EBind(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.delimc.Absyn.ENewPrompt p, A arg)
    {
      return new rholang.parsing.delimc.Absyn.ENewPrompt();
    }    public Expr visit(rholang.parsing.delimc.Absyn.EPushPrompt p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EPushPrompt(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EWithSubCont p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EWithSubCont(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.delimc.Absyn.EPushSubCont p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.EPushSubCont(typedexpr_1, typedexpr_2);
    }    public Expr visit(rholang.parsing.delimc.Absyn.ETuple p, A arg)
    {
      Tuple tuple_ = p.tuple_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.ETuple(tuple_);
    }
/* Tuple */
    public Tuple visit(rholang.parsing.delimc.Absyn.Tuple2 p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.Tuple2(typedexpr_1, typedexpr_2);
    }    public Tuple visit(rholang.parsing.delimc.Absyn.Tuple3 p, A arg)
    {
      TypedExpr typedexpr_1 = p.typedexpr_1.accept(this, arg);
      TypedExpr typedexpr_2 = p.typedexpr_2.accept(this, arg);
      TypedExpr typedexpr_3 = p.typedexpr_3.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.Tuple3(typedexpr_1, typedexpr_2, typedexpr_3);
    }
/* Value */
    public Value visit(rholang.parsing.delimc.Absyn.VInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new rholang.parsing.delimc.Absyn.VInt(integer_);
    }    public Value visit(rholang.parsing.delimc.Absyn.VString p, A arg)
    {
      String string_ = p.string_;
      return new rholang.parsing.delimc.Absyn.VString(string_);
    }
/* Type */
    public Type visit(rholang.parsing.delimc.Absyn.TSimple p, A arg)
    {
      String simpletype_ = p.simpletype_;
      return new rholang.parsing.delimc.Absyn.TSimple(simpletype_);
    }    public Type visit(rholang.parsing.delimc.Absyn.TTuple p, A arg)
    {
      TType ttype_ = p.ttype_.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.TTuple(ttype_);
    }    public Type visit(rholang.parsing.delimc.Absyn.TMonad p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.TMonad(type_1, type_2);
    }    public Type visit(rholang.parsing.delimc.Absyn.TFun p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.TFun(type_1, type_2);
    }
/* TType */
    public TType visit(rholang.parsing.delimc.Absyn.TType2 p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.TType2(type_1, type_2);
    }    public TType visit(rholang.parsing.delimc.Absyn.TType3 p, A arg)
    {
      Type type_1 = p.type_1.accept(this, arg);
      Type type_2 = p.type_2.accept(this, arg);
      Type type_3 = p.type_3.accept(this, arg);
      return new rholang.parsing.delimc.Absyn.TType3(type_1, type_2, type_3);
    }
}