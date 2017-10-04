package rholang.parsing.rholang1;

import rholang.parsing.rholang1.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Contr */
    public R visit(rholang.parsing.rholang1.Absyn.DContr p, A arg) {
      R r = leaf(arg);
      for (Pattern x : p.listpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }

/* Expr */
    public R visit(rholang.parsing.rholang1.Absyn.ENil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EValue p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.ELift p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      for (Expr x : p.listexpr_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EInput p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EChoice p, A arg) {
      R r = leaf(arg);
      for (CBranch x : p.listcbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      for (PMBranch x : p.listpmbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.ENew p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EConstr p, A arg) {
      R r = leaf(arg);
      for (Expr x : p.listexpr_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.expr_1.accept(this, arg), r, arg);
      r = combine(p.expr_2.accept(this, arg), r, arg);
      return r;
    }

/* Bind */
    public R visit(rholang.parsing.rholang1.Absyn.InputBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }

/* PMBranch */
    public R visit(rholang.parsing.rholang1.Absyn.PatternMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }

/* CBranch */
    public R visit(rholang.parsing.rholang1.Absyn.Choice p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.expr_.accept(this, arg), r, arg);
      return r;
    }

/* Value */
    public R visit(rholang.parsing.rholang1.Absyn.VQuant p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.VEnt p, A arg) {
      R r = leaf(arg);
      r = combine(p.entity_.accept(this, arg), r, arg);
      return r;
    }

/* Quantity */
    public R visit(rholang.parsing.rholang1.Absyn.QInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.QDouble p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Entity */
    public R visit(rholang.parsing.rholang1.Absyn.EChar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.EStruct p, A arg) {
      R r = leaf(arg);
      r = combine(p.struct_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.ECollect p, A arg) {
      R r = leaf(arg);
      r = combine(p.collect_.accept(this, arg), r, arg);
      return r;
    }

/* Struct */
    public R visit(rholang.parsing.rholang1.Absyn.StructConstr p, A arg) {
      R r = leaf(arg);
      for (Expr x : p.listexpr_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }

/* Collect */
    public R visit(rholang.parsing.rholang1.Absyn.CString p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* VarPattern */
    public R visit(rholang.parsing.rholang1.Absyn.VarPtVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.VarPtWild p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Pattern */
    public R visit(rholang.parsing.rholang1.Absyn.PtNil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.varpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.valpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtOutput p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      for (Pattern x : p.listpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtInput p, A arg) {
      R r = leaf(arg);
      for (PatternBind x : p.listpatternbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      for (PatternPatternMatch x : p.listpatternpatternmatch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtNew p, A arg) {
      R r = leaf(arg);
      for (VarPattern x : p.listvarpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.pattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtConstr p, A arg) {
      R r = leaf(arg);
      for (Pattern x : p.listpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang1.Absyn.PtPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_1.accept(this, arg), r, arg);
      r = combine(p.pattern_2.accept(this, arg), r, arg);
      return r;
    }

/* PatternBind */
    public R visit(rholang.parsing.rholang1.Absyn.PtBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_1.accept(this, arg), r, arg);
      r = combine(p.pattern_2.accept(this, arg), r, arg);
      return r;
    }

/* PatternPatternMatch */
    public R visit(rholang.parsing.rholang1.Absyn.PtBranch p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_1.accept(this, arg), r, arg);
      r = combine(p.pattern_2.accept(this, arg), r, arg);
      return r;
    }

/* ValPattern */
    public R visit(rholang.parsing.rholang1.Absyn.VPtStruct p, A arg) {
      R r = leaf(arg);
      for (Pattern x : p.listpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }


}
