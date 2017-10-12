package rholang.parsing.rholang1;
import rholang.parsing.rholang1.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  rholang.parsing.rholang1.Absyn.Contr.Visitor<rholang.parsing.rholang1.Absyn.Contr,A>,
  rholang.parsing.rholang1.Absyn.Expr.Visitor<rholang.parsing.rholang1.Absyn.Expr,A>,
  rholang.parsing.rholang1.Absyn.Bind.Visitor<rholang.parsing.rholang1.Absyn.Bind,A>,
  rholang.parsing.rholang1.Absyn.PMBranch.Visitor<rholang.parsing.rholang1.Absyn.PMBranch,A>,
  rholang.parsing.rholang1.Absyn.CBranch.Visitor<rholang.parsing.rholang1.Absyn.CBranch,A>,
  rholang.parsing.rholang1.Absyn.Value.Visitor<rholang.parsing.rholang1.Absyn.Value,A>,
  rholang.parsing.rholang1.Absyn.Quantity.Visitor<rholang.parsing.rholang1.Absyn.Quantity,A>,
  rholang.parsing.rholang1.Absyn.Entity.Visitor<rholang.parsing.rholang1.Absyn.Entity,A>,
  rholang.parsing.rholang1.Absyn.Struct.Visitor<rholang.parsing.rholang1.Absyn.Struct,A>,
  rholang.parsing.rholang1.Absyn.Collect.Visitor<rholang.parsing.rholang1.Absyn.Collect,A>,
  rholang.parsing.rholang1.Absyn.VarPattern.Visitor<rholang.parsing.rholang1.Absyn.VarPattern,A>,
  rholang.parsing.rholang1.Absyn.Pattern.Visitor<rholang.parsing.rholang1.Absyn.Pattern,A>,
  rholang.parsing.rholang1.Absyn.PatternBind.Visitor<rholang.parsing.rholang1.Absyn.PatternBind,A>,
  rholang.parsing.rholang1.Absyn.PatternPatternMatch.Visitor<rholang.parsing.rholang1.Absyn.PatternPatternMatch,A>,
  rholang.parsing.rholang1.Absyn.ValPattern.Visitor<rholang.parsing.rholang1.Absyn.ValPattern,A>
{
/* Contr */
    public Contr visit(rholang.parsing.rholang1.Absyn.DContr p, A arg)
    {
      String name_ = p.name_;
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_)
      {
        listpattern_.add(x.accept(this,arg));
      }
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.DContr(name_, listpattern_, expr_);
    }
/* Expr */
    public Expr visit(rholang.parsing.rholang1.Absyn.ENil p, A arg)
    {
      return new rholang.parsing.rholang1.Absyn.ENil();
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EValue p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EValue(value_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.rholang1.Absyn.EVar(var_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EDrop p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EDrop(expr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EQuote p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EQuote(expr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EInject p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EInject(expr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.ELift p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      ListExpr listexpr_ = new ListExpr();
      for (Expr x : p.listexpr_)
      {
        listexpr_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.ELift(expr_, listexpr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EInput p, A arg)
    {
      ListBind listbind_ = new ListBind();
      for (Bind x : p.listbind_)
      {
        listbind_.add(x.accept(this,arg));
      }
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EInput(listbind_, expr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EChoice p, A arg)
    {
      ListCBranch listcbranch_ = new ListCBranch();
      for (CBranch x : p.listcbranch_)
      {
        listcbranch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.EChoice(listcbranch_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EMatch p, A arg)
    {
      Expr expr_ = p.expr_.accept(this, arg);
      ListPMBranch listpmbranch_ = new ListPMBranch();
      for (PMBranch x : p.listpmbranch_)
      {
        listpmbranch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.EMatch(expr_, listpmbranch_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.ENew p, A arg)
    {
      ListVar listvar_ = p.listvar_;
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.ENew(listvar_, expr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EConstr p, A arg)
    {
      String name_ = p.name_;
      ListExpr listexpr_ = new ListExpr();
      for (Expr x : p.listexpr_)
      {
        listexpr_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.EConstr(name_, listexpr_);
    }    public Expr visit(rholang.parsing.rholang1.Absyn.EPar p, A arg)
    {
      Expr expr_1 = p.expr_1.accept(this, arg);
      Expr expr_2 = p.expr_2.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EPar(expr_1, expr_2);
    }
/* Bind */
    public Bind visit(rholang.parsing.rholang1.Absyn.InputBind p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.InputBind(pattern_, expr_);
    }
/* PMBranch */
    public PMBranch visit(rholang.parsing.rholang1.Absyn.PatternMatch p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PatternMatch(pattern_, expr_);
    }
/* CBranch */
    public CBranch visit(rholang.parsing.rholang1.Absyn.Choice p, A arg)
    {
      ListBind listbind_ = new ListBind();
      for (Bind x : p.listbind_)
      {
        listbind_.add(x.accept(this,arg));
      }
      Expr expr_ = p.expr_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.Choice(listbind_, expr_);
    }
/* Value */
    public Value visit(rholang.parsing.rholang1.Absyn.VQuant p, A arg)
    {
      Quantity quantity_ = p.quantity_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.VQuant(quantity_);
    }    public Value visit(rholang.parsing.rholang1.Absyn.VEnt p, A arg)
    {
      Entity entity_ = p.entity_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.VEnt(entity_);
    }
/* Quantity */
    public Quantity visit(rholang.parsing.rholang1.Absyn.QInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new rholang.parsing.rholang1.Absyn.QInt(integer_);
    }    public Quantity visit(rholang.parsing.rholang1.Absyn.QDouble p, A arg)
    {
      Double double_ = p.double_;
      return new rholang.parsing.rholang1.Absyn.QDouble(double_);
    }
/* Entity */
    public Entity visit(rholang.parsing.rholang1.Absyn.EChar p, A arg)
    {
      Character char_ = p.char_;
      return new rholang.parsing.rholang1.Absyn.EChar(char_);
    }    public Entity visit(rholang.parsing.rholang1.Absyn.EStruct p, A arg)
    {
      Struct struct_ = p.struct_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.EStruct(struct_);
    }    public Entity visit(rholang.parsing.rholang1.Absyn.ECollect p, A arg)
    {
      Collect collect_ = p.collect_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.ECollect(collect_);
    }
/* Struct */
    public Struct visit(rholang.parsing.rholang1.Absyn.StructConstr p, A arg)
    {
      String var_ = p.var_;
      ListExpr listexpr_ = new ListExpr();
      for (Expr x : p.listexpr_)
      {
        listexpr_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.StructConstr(var_, listexpr_);
    }
/* Collect */
    public Collect visit(rholang.parsing.rholang1.Absyn.CString p, A arg)
    {
      String string_ = p.string_;
      return new rholang.parsing.rholang1.Absyn.CString(string_);
    }
/* VarPattern */
    public VarPattern visit(rholang.parsing.rholang1.Absyn.VarPtVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.rholang1.Absyn.VarPtVar(var_);
    }    public VarPattern visit(rholang.parsing.rholang1.Absyn.VarPtWild p, A arg)
    {
      return new rholang.parsing.rholang1.Absyn.VarPtWild();
    }
/* Pattern */
    public Pattern visit(rholang.parsing.rholang1.Absyn.PtNil p, A arg)
    {
      return new rholang.parsing.rholang1.Absyn.PtNil();
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtVar p, A arg)
    {
      VarPattern varpattern_ = p.varpattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtVar(varpattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtVal p, A arg)
    {
      ValPattern valpattern_ = p.valpattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtVal(valpattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtDrop p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtDrop(pattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtInject p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtInject(pattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtQuote p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtQuote(pattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtOutput p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_)
      {
        listpattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.PtOutput(pattern_, listpattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtInput p, A arg)
    {
      ListPatternBind listpatternbind_ = new ListPatternBind();
      for (PatternBind x : p.listpatternbind_)
      {
        listpatternbind_.add(x.accept(this,arg));
      }
      Pattern pattern_ = p.pattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtInput(listpatternbind_, pattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtMatch p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      ListPatternPatternMatch listpatternpatternmatch_ = new ListPatternPatternMatch();
      for (PatternPatternMatch x : p.listpatternpatternmatch_)
      {
        listpatternpatternmatch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.PtMatch(pattern_, listpatternpatternmatch_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtNew p, A arg)
    {
      ListVarPattern listvarpattern_ = new ListVarPattern();
      for (VarPattern x : p.listvarpattern_)
      {
        listvarpattern_.add(x.accept(this,arg));
      }
      Pattern pattern_ = p.pattern_.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtNew(listvarpattern_, pattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtConstr p, A arg)
    {
      String name_ = p.name_;
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_)
      {
        listpattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.PtConstr(name_, listpattern_);
    }    public Pattern visit(rholang.parsing.rholang1.Absyn.PtPar p, A arg)
    {
      Pattern pattern_1 = p.pattern_1.accept(this, arg);
      Pattern pattern_2 = p.pattern_2.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtPar(pattern_1, pattern_2);
    }
/* PatternBind */
    public PatternBind visit(rholang.parsing.rholang1.Absyn.PtBind p, A arg)
    {
      Pattern pattern_1 = p.pattern_1.accept(this, arg);
      Pattern pattern_2 = p.pattern_2.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtBind(pattern_1, pattern_2);
    }
/* PatternPatternMatch */
    public PatternPatternMatch visit(rholang.parsing.rholang1.Absyn.PtBranch p, A arg)
    {
      Pattern pattern_1 = p.pattern_1.accept(this, arg);
      Pattern pattern_2 = p.pattern_2.accept(this, arg);
      return new rholang.parsing.rholang1.Absyn.PtBranch(pattern_1, pattern_2);
    }
/* ValPattern */
    public ValPattern visit(rholang.parsing.rholang1.Absyn.VPtStruct p, A arg)
    {
      String var_ = p.var_;
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_)
      {
        listpattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang1.Absyn.VPtStruct(var_, listpattern_);
    }
}