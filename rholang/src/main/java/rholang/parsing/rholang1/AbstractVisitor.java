package rholang.parsing.rholang1;
import rholang.parsing.rholang1.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Contr */
    public R visit(rholang.parsing.rholang1.Absyn.DContr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Contr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Expr */
    public R visit(rholang.parsing.rholang1.Absyn.ENil p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EValue p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EVar p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.EDrop p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EQuote p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EInject p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.ELift p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.EInput p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EChoice p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EMatch p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.ENew p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EConstr p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.EPar p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.rholang1.Absyn.Expr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Bind */
    public R visit(rholang.parsing.rholang1.Absyn.InputBind p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Bind p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PMBranch */
    public R visit(rholang.parsing.rholang1.Absyn.PatternMatch p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.PMBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* CBranch */
    public R visit(rholang.parsing.rholang1.Absyn.Choice p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.CBranch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(rholang.parsing.rholang1.Absyn.VQuant p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.VEnt p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Quantity */
    public R visit(rholang.parsing.rholang1.Absyn.QInt p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.QDouble p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Quantity p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Entity */
    public R visit(rholang.parsing.rholang1.Absyn.EChar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.EStruct p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.ECollect p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Entity p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Struct */
    public R visit(rholang.parsing.rholang1.Absyn.StructConstr p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Struct p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Collect */
    public R visit(rholang.parsing.rholang1.Absyn.CString p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.Collect p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* VarPattern */
    public R visit(rholang.parsing.rholang1.Absyn.VarPtVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.VarPtWild p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.VarPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Pattern */
    public R visit(rholang.parsing.rholang1.Absyn.PtNil p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtVar p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtVal p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.PtDrop p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtInject p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtQuote p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.PtOutput p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.PtInput p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtMatch p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtNew p, A arg) { return visitDefault(p, arg); }
    public R visit(rholang.parsing.rholang1.Absyn.PtConstr p, A arg) { return visitDefault(p, arg); }

    public R visit(rholang.parsing.rholang1.Absyn.PtPar p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(rholang.parsing.rholang1.Absyn.Pattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PatternBind */
    public R visit(rholang.parsing.rholang1.Absyn.PtBind p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.PatternBind p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* PatternPatternMatch */
    public R visit(rholang.parsing.rholang1.Absyn.PtBranch p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.PatternPatternMatch p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValPattern */
    public R visit(rholang.parsing.rholang1.Absyn.VPtStruct p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(rholang.parsing.rholang1.Absyn.ValPattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
