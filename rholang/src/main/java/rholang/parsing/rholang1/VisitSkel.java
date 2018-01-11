package rholang.parsing.rholang1;
import rholang.parsing.rholang1.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class ContrVisitor<R,A> implements Contr.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.DContr p, A arg)
    { /* Code For DContr Goes Here */
      //p.name_;
      for (Pattern x: p.listpattern_)
      { /* ... */ }
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ExprVisitor<R,A> implements Expr.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.ENil p, A arg)
    { /* Code For ENil Goes Here */
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EValue p, A arg)
    { /* Code For EValue Goes Here */
      p.value_.accept(new ValueVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EVar p, A arg)
    { /* Code For EVar Goes Here */
      //p.var_;
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.EDrop p, A arg)
    { /* Code For EDrop Goes Here */
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EQuote p, A arg)
    { /* Code For EQuote Goes Here */
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EInject p, A arg)
    { /* Code For EInject Goes Here */
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.ELift p, A arg)
    { /* Code For ELift Goes Here */
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      for (Expr x: p.listexpr_)
      { /* ... */ }
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.EInput p, A arg)
    { /* Code For EInput Goes Here */
      for (Bind x: p.listbind_)
      { /* ... */ }
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EChoice p, A arg)
    { /* Code For EChoice Goes Here */
      for (CBranch x: p.listcbranch_)
      { /* ... */ }
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EMatch p, A arg)
    { /* Code For EMatch Goes Here */
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      for (PMBranch x: p.listpmbranch_)
      { /* ... */ }
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.ENew p, A arg)
    { /* Code For ENew Goes Here */
      for (String x: p.listvar_)
      { /* ... */ }
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EConstr p, A arg)
    { /* Code For EConstr Goes Here */
      //p.name_;
      for (Expr x: p.listexpr_)
      { /* ... */ }
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.EPar p, A arg)
    { /* Code For EPar Goes Here */
      p.expr_1.accept(new ExprVisitor<R,A>(), arg);
      p.expr_2.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }    
  }
  public class BindVisitor<R,A> implements Bind.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.InputBind p, A arg)
    { /* Code For InputBind Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }
  }
  public class PMBranchVisitor<R,A> implements PMBranch.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.PatternMatch p, A arg)
    { /* Code For PatternMatch Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }
  }
  public class CBranchVisitor<R,A> implements CBranch.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.Choice p, A arg)
    { /* Code For Choice Goes Here */
      for (Bind x: p.listbind_)
      { /* ... */ }
      p.expr_.accept(new ExprVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ValueVisitor<R,A> implements Value.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.VQuant p, A arg)
    { /* Code For VQuant Goes Here */
      p.quantity_.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.VEnt p, A arg)
    { /* Code For VEnt Goes Here */
      p.entity_.accept(new EntityVisitor<R,A>(), arg);
      return null;
    }
  }
  public class QuantityVisitor<R,A> implements Quantity.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.QInt p, A arg)
    { /* Code For QInt Goes Here */
      //p.integer_;
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.QDouble p, A arg)
    { /* Code For QDouble Goes Here */
      //p.double_;
      return null;
    }
  }
  public class EntityVisitor<R,A> implements Entity.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.EChar p, A arg)
    { /* Code For EChar Goes Here */
      //p.char_;
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.EStruct p, A arg)
    { /* Code For EStruct Goes Here */
      p.struct_.accept(new StructVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.ECollect p, A arg)
    { /* Code For ECollect Goes Here */
      p.collect_.accept(new CollectVisitor<R,A>(), arg);
      return null;
    }
  }
  public class StructVisitor<R,A> implements Struct.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.StructConstr p, A arg)
    { /* Code For StructConstr Goes Here */
      //p.var_;
      for (Expr x: p.listexpr_)
      { /* ... */ }
      return null;
    }
  }
  public class CollectVisitor<R,A> implements Collect.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.CString p, A arg)
    { /* Code For CString Goes Here */
      //p.string_;
      return null;
    }
  }
  public class VarPatternVisitor<R,A> implements VarPattern.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.VarPtVar p, A arg)
    { /* Code For VarPtVar Goes Here */
      //p.var_;
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.VarPtWild p, A arg)
    { /* Code For VarPtWild Goes Here */
      return null;
    }
  }
  public class PatternVisitor<R,A> implements Pattern.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.PtNil p, A arg)
    { /* Code For PtNil Goes Here */
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtVar p, A arg)
    { /* Code For PtVar Goes Here */
      p.varpattern_.accept(new VarPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtVal p, A arg)
    { /* Code For PtVal Goes Here */
      p.valpattern_.accept(new ValPatternVisitor<R,A>(), arg);
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.PtDrop p, A arg)
    { /* Code For PtDrop Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtInject p, A arg)
    { /* Code For PtInject Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtQuote p, A arg)
    { /* Code For PtQuote Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.PtOutput p, A arg)
    { /* Code For PtOutput Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      for (Pattern x: p.listpattern_)
      { /* ... */ }
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.PtInput p, A arg)
    { /* Code For PtInput Goes Here */
      for (PatternBind x: p.listpatternbind_)
      { /* ... */ }
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtMatch p, A arg)
    { /* Code For PtMatch Goes Here */
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      for (PatternPatternMatch x: p.listpatternpatternmatch_)
      { /* ... */ }
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtNew p, A arg)
    { /* Code For PtNew Goes Here */
      for (VarPattern x: p.listvarpattern_)
      { /* ... */ }
      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(rholang.parsing.rholang1.Absyn.PtConstr p, A arg)
    { /* Code For PtConstr Goes Here */
      //p.name_;
      for (Pattern x: p.listpattern_)
      { /* ... */ }
      return null;
    }        public R visit(rholang.parsing.rholang1.Absyn.PtPar p, A arg)
    { /* Code For PtPar Goes Here */
      p.pattern_1.accept(new PatternVisitor<R,A>(), arg);
      p.pattern_2.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }    
  }
  public class PatternBindVisitor<R,A> implements PatternBind.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.PtBind p, A arg)
    { /* Code For PtBind Goes Here */
      p.pattern_1.accept(new PatternVisitor<R,A>(), arg);
      p.pattern_2.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }
  }
  public class PatternPatternMatchVisitor<R,A> implements PatternPatternMatch.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.PtBranch p, A arg)
    { /* Code For PtBranch Goes Here */
      p.pattern_1.accept(new PatternVisitor<R,A>(), arg);
      p.pattern_2.accept(new PatternVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ValPatternVisitor<R,A> implements ValPattern.Visitor<R,A>
  {
    public R visit(rholang.parsing.rholang1.Absyn.VPtStruct p, A arg)
    { /* Code For VPtStruct Goes Here */
      //p.var_;
      for (Pattern x: p.listpattern_)
      { /* ... */ }
      return null;
    }
  }
}