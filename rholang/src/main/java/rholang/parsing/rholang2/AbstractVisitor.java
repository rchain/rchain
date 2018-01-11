package rholang.parsing.rholang2;
import rholang.parsing.rholang2.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Contr */
    public R visit(rholang.parsing.rholang2.Absyn.DContr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Contr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Proc */
    public R visit(rholang.parsing.rholang2.Absyn.PNil p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PValue p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PVar p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PDrop p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PInject p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PLift p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PInput p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PChoice p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PMatch p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PNew p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PConstr p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PPar p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.rholang2.Absyn.Proc p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Chan */
    public R visit(rholang.parsing.rholang2.Absyn.CVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.CQuote p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Chan p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Bind */
    public R visit(rholang.parsing.rholang2.Absyn.InputBind p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Bind p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PMBranch */
    public R visit(rholang.parsing.rholang2.Absyn.PatternMatch p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.PMBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* CBranch */
    public R visit(rholang.parsing.rholang2.Absyn.Choice p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.CBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(rholang.parsing.rholang2.Absyn.VQuant p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.VEnt p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Quantity */
    public R visit(rholang.parsing.rholang2.Absyn.QInt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.QDouble p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Quantity p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Entity */
    public R visit(rholang.parsing.rholang2.Absyn.EChar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.EStruct p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.ECollect p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Entity p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Struct */
    public R visit(rholang.parsing.rholang2.Absyn.StructConstr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Struct p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Collect */
    public R visit(rholang.parsing.rholang2.Absyn.CString p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.Collect p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* VarPattern */
    public R visit(rholang.parsing.rholang2.Absyn.VarPtVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.VarPtWild p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.VarPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PPattern */
    public R visit(rholang.parsing.rholang2.Absyn.PPtVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtNil p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtVal p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PPtDrop p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtInject p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PPtOutput p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PPtInput p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtMatch p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtNew p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.PPtConstr p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang2.Absyn.PPtPar p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.rholang2.Absyn.PPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* CPattern */
    public R visit(rholang.parsing.rholang2.Absyn.CPtVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang2.Absyn.CPtQuote p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.CPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PatternBind */
    public R visit(rholang.parsing.rholang2.Absyn.PtBind p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.PatternBind p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PatternPatternMatch */
    public R visit(rholang.parsing.rholang2.Absyn.PtBranch p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.PatternPatternMatch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValPattern */
    public R visit(rholang.parsing.rholang2.Absyn.VPtStruct p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang2.Absyn.ValPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
