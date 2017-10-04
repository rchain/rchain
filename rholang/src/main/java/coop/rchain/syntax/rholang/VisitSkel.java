package coop.rchain.syntax.rholang;
import coop.rchain.syntax.rholang.Absyn.*;
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
    public R visit(coop.rchain.syntax.rholang.Absyn.DContr p, A arg)
    { /* Code For DContr Goes Here */
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ProcVisitor<R,A> implements Proc.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.PNil p, A arg)
    { /* Code For PNil Goes Here */
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PValue p, A arg)
    { /* Code For PValue Goes Here */
      p.value_.accept(new ValueVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PDrop p, A arg)
    { /* Code For PDrop Goes Here */
      p.chan_.accept(new ChanVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PInject p, A arg)
    { /* Code For PInject Goes Here */
      p.chan_.accept(new ChanVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PLift p, A arg)
    { /* Code For PLift Goes Here */
      p.chan_.accept(new ChanVisitor<R,A>(), arg);
      for (Proc x: p.listproc_)
      { /* ... */ }
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PFoldL p, A arg)
    { /* Code For PFoldL Goes Here */
      p.bind_1.accept(new BindVisitor<R,A>(), arg);
      p.bind_2.accept(new BindVisitor<R,A>(), arg);
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PFoldR p, A arg)
    { /* Code For PFoldR Goes Here */
      p.bind_1.accept(new BindVisitor<R,A>(), arg);
      p.bind_2.accept(new BindVisitor<R,A>(), arg);
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PInput p, A arg)
    { /* Code For PInput Goes Here */
      for (Bind x: p.listbind_)
      { /* ... */ }
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PChoice p, A arg)
    { /* Code For PChoice Goes Here */
      for (CBranch x: p.listcbranch_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PMatch p, A arg)
    { /* Code For PMatch Goes Here */
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      for (PMBranch x: p.listpmbranch_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PNew p, A arg)
    { /* Code For PNew Goes Here */
      for (String x: p.listvar_)
      { /* ... */ }
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPrint p, A arg)
    { /* Code For PPrint Goes Here */
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PConstr p, A arg)
    { /* Code For PConstr Goes Here */
      //p.var_;
      for (Proc x: p.listproc_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PContr p, A arg)
    { /* Code For PContr Goes Here */
      //p.var_;
      for (CPattern x: p.listcpattern_)
      { /* ... */ }
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PPar p, A arg)
    { /* Code For PPar Goes Here */
      p.proc_1.accept(new ProcVisitor<R,A>(), arg);
      p.proc_2.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }    
  }
  public class ChanVisitor<R,A> implements Chan.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.CVar p, A arg)
    { /* Code For CVar Goes Here */
      //p.var_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.CQuote p, A arg)
    { /* Code For CQuote Goes Here */
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }
  }
  public class BindVisitor<R,A> implements Bind.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.InputBind p, A arg)
    { /* Code For InputBind Goes Here */
      p.cpattern_.accept(new CPatternVisitor<R,A>(), arg);
      p.chan_.accept(new ChanVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.CondInputBind p, A arg)
    { /* Code For CondInputBind Goes Here */
      p.cpattern_.accept(new CPatternVisitor<R,A>(), arg);
      p.chan_.accept(new ChanVisitor<R,A>(), arg);
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }
  }
  public class PMBranchVisitor<R,A> implements PMBranch.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.PatternMatch p, A arg)
    { /* Code For PatternMatch Goes Here */
      p.ppattern_.accept(new PPatternVisitor<R,A>(), arg);
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }
  }
  public class CBranchVisitor<R,A> implements CBranch.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.Choice p, A arg)
    { /* Code For Choice Goes Here */
      for (Bind x: p.listbind_)
      { /* ... */ }
      p.proc_.accept(new ProcVisitor<R,A>(), arg);
      return null;
    }
  }
  public class RhoBoolVisitor<R,A> implements RhoBool.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.QTrue p, A arg)
    { /* Code For QTrue Goes Here */
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QFalse p, A arg)
    { /* Code For QFalse Goes Here */
      return null;
    }
  }
  public class QuantityVisitor<R,A> implements Quantity.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.QBool p, A arg)
    { /* Code For QBool Goes Here */
      p.rhobool_.accept(new RhoBoolVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QInt p, A arg)
    { /* Code For QInt Goes Here */
      //p.integer_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QDouble p, A arg)
    { /* Code For QDouble Goes Here */
      //p.double_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QString p, A arg)
    { /* Code For QString Goes Here */
      //p.string_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QVar p, A arg)
    { /* Code For QVar Goes Here */
      //p.var_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QMap p, A arg)
    { /* Code For QMap Goes Here */
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QDot p, A arg)
    { /* Code For QDot Goes Here */
      p.quantity_.accept(new QuantityVisitor<R,A>(), arg);
      //p.var_;
      for (Quantity x: p.listquantity_)
      { /* ... */ }
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QNeg p, A arg)
    { /* Code For QNeg Goes Here */
      p.quantity_.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QMult p, A arg)
    { /* Code For QMult Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QDiv p, A arg)
    { /* Code For QDiv Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QAdd p, A arg)
    { /* Code For QAdd Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QMinus p, A arg)
    { /* Code For QMinus Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QLt p, A arg)
    { /* Code For QLt Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QLte p, A arg)
    { /* Code For QLte Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QGt p, A arg)
    { /* Code For QGt Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QGte p, A arg)
    { /* Code For QGte Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.QEq p, A arg)
    { /* Code For QEq Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.QNeq p, A arg)
    { /* Code For QNeq Goes Here */
      p.quantity_1.accept(new QuantityVisitor<R,A>(), arg);
      p.quantity_2.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }        
  }
  public class ValueVisitor<R,A> implements Value.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.VQuant p, A arg)
    { /* Code For VQuant Goes Here */
      p.quantity_.accept(new QuantityVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.EChar p, A arg)
    { /* Code For EChar Goes Here */
      //p.char_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.ETuple p, A arg)
    { /* Code For ETuple Goes Here */
      for (Proc x: p.listproc_)
      { /* ... */ }
      return null;
    }
  }
  public class VarPatternVisitor<R,A> implements VarPattern.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.VarPtVar p, A arg)
    { /* Code For VarPtVar Goes Here */
      //p.var_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VarPtWild p, A arg)
    { /* Code For VarPtWild Goes Here */
      return null;
    }
  }
  public class PPatternVisitor<R,A> implements PPattern.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtVar p, A arg)
    { /* Code For PPtVar Goes Here */
      p.varpattern_.accept(new VarPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtNil p, A arg)
    { /* Code For PPtNil Goes Here */
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtVal p, A arg)
    { /* Code For PPtVal Goes Here */
      p.valpattern_.accept(new ValPatternVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PPtDrop p, A arg)
    { /* Code For PPtDrop Goes Here */
      p.cpattern_.accept(new CPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtInject p, A arg)
    { /* Code For PPtInject Goes Here */
      p.cpattern_.accept(new CPatternVisitor<R,A>(), arg);
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PPtOutput p, A arg)
    { /* Code For PPtOutput Goes Here */
      p.cpattern_.accept(new CPatternVisitor<R,A>(), arg);
      for (PPattern x: p.listppattern_)
      { /* ... */ }
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PPtInput p, A arg)
    { /* Code For PPtInput Goes Here */
      for (PatternBind x: p.listpatternbind_)
      { /* ... */ }
      p.ppattern_.accept(new PPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtMatch p, A arg)
    { /* Code For PPtMatch Goes Here */
      p.ppattern_.accept(new PPatternVisitor<R,A>(), arg);
      for (PatternPatternMatch x: p.listpatternpatternmatch_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtNew p, A arg)
    { /* Code For PPtNew Goes Here */
      for (VarPattern x: p.listvarpattern_)
      { /* ... */ }
      p.ppattern_.accept(new PPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.PPtConstr p, A arg)
    { /* Code For PPtConstr Goes Here */
      //p.var_;
      for (PPattern x: p.listppattern_)
      { /* ... */ }
      return null;
    }        public R visit(coop.rchain.syntax.rholang.Absyn.PPtPar p, A arg)
    { /* Code For PPtPar Goes Here */
      p.ppattern_1.accept(new PPatternVisitor<R,A>(), arg);
      p.ppattern_2.accept(new PPatternVisitor<R,A>(), arg);
      return null;
    }    
  }
  public class CPatternVisitor<R,A> implements CPattern.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.CPtVar p, A arg)
    { /* Code For CPtVar Goes Here */
      p.varpattern_.accept(new VarPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.CValPtrn p, A arg)
    { /* Code For CValPtrn Goes Here */
      p.valpattern_.accept(new ValPatternVisitor<R,A>(), arg);
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.CPtQuote p, A arg)
    { /* Code For CPtQuote Goes Here */
      p.ppattern_.accept(new PPatternVisitor<R,A>(), arg);
      return null;
    }
  }
  public class PatternBindVisitor<R,A> implements PatternBind.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.PtBind p, A arg)
    { /* Code For PtBind Goes Here */
      p.cpattern_1.accept(new CPatternVisitor<R,A>(), arg);
      p.cpattern_2.accept(new CPatternVisitor<R,A>(), arg);
      return null;
    }
  }
  public class PatternPatternMatchVisitor<R,A> implements PatternPatternMatch.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.PtBranch p, A arg)
    { /* Code For PtBranch Goes Here */
      p.ppattern_1.accept(new PPatternVisitor<R,A>(), arg);
      p.ppattern_2.accept(new PPatternVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ValPatternVisitor<R,A> implements ValPattern.Visitor<R,A>
  {
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStruct p, A arg)
    { /* Code For VPtStruct Goes Here */
      //p.var_;
      for (PPattern x: p.listppattern_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTuple p, A arg)
    { /* Code For VPtTuple Goes Here */
      for (PPattern x: p.listppattern_)
      { /* ... */ }
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTrue p, A arg)
    { /* Code For VPtTrue Goes Here */
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtFalse p, A arg)
    { /* Code For VPtFalse Goes Here */
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtInt p, A arg)
    { /* Code For VPtInt Goes Here */
      //p.integer_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtDbl p, A arg)
    { /* Code For VPtDbl Goes Here */
      //p.double_;
      return null;
    }    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStr p, A arg)
    { /* Code For VPtStr Goes Here */
      //p.string_;
      return null;
    }
  }
}