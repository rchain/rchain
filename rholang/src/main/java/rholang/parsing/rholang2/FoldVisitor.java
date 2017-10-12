package rholang.parsing.rholang2;

import rholang.parsing.rholang2.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Contr */
    public R visit(rholang.parsing.rholang2.Absyn.DContr p, A arg) {
      R r = leaf(arg);
      for (CPattern x : p.listcpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* Proc */
    public R visit(rholang.parsing.rholang2.Absyn.PNil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PValue p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PLift p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PInput p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PChoice p, A arg) {
      R r = leaf(arg);
      for (CBranch x : p.listcbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      for (PMBranch x : p.listpmbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PNew p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PConstr p, A arg) {
      R r = leaf(arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_1.accept(this, arg), r, arg);
      r = combine(p.proc_2.accept(this, arg), r, arg);
      return r;
    }

/* Chan */
    public R visit(rholang.parsing.rholang2.Absyn.CVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.CQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* Bind */
    public R visit(rholang.parsing.rholang2.Absyn.InputBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }

/* PMBranch */
    public R visit(rholang.parsing.rholang2.Absyn.PatternMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* CBranch */
    public R visit(rholang.parsing.rholang2.Absyn.Choice p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* Value */
    public R visit(rholang.parsing.rholang2.Absyn.VQuant p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.VEnt p, A arg) {
      R r = leaf(arg);
      r = combine(p.entity_.accept(this, arg), r, arg);
      return r;
    }

/* Quantity */
    public R visit(rholang.parsing.rholang2.Absyn.QInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.QDouble p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Entity */
    public R visit(rholang.parsing.rholang2.Absyn.EChar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.EStruct p, A arg) {
      R r = leaf(arg);
      r = combine(p.struct_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.ECollect p, A arg) {
      R r = leaf(arg);
      r = combine(p.collect_.accept(this, arg), r, arg);
      return r;
    }

/* Struct */
    public R visit(rholang.parsing.rholang2.Absyn.StructConstr p, A arg) {
      R r = leaf(arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }

/* Collect */
    public R visit(rholang.parsing.rholang2.Absyn.CString p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* VarPattern */
    public R visit(rholang.parsing.rholang2.Absyn.VarPtVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.VarPtWild p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* PPattern */
    public R visit(rholang.parsing.rholang2.Absyn.PPtVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.varpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtNil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.valpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtOutput p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtInput p, A arg) {
      R r = leaf(arg);
      for (PatternBind x : p.listpatternbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      for (PatternPatternMatch x : p.listpatternpatternmatch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtNew p, A arg) {
      R r = leaf(arg);
      for (VarPattern x : p.listvarpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtConstr p, A arg) {
      R r = leaf(arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.PPtPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_1.accept(this, arg), r, arg);
      r = combine(p.ppattern_2.accept(this, arg), r, arg);
      return r;
    }

/* CPattern */
    public R visit(rholang.parsing.rholang2.Absyn.CPtVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.varpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(rholang.parsing.rholang2.Absyn.CPtQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }

/* PatternBind */
    public R visit(rholang.parsing.rholang2.Absyn.PtBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_1.accept(this, arg), r, arg);
      r = combine(p.cpattern_2.accept(this, arg), r, arg);
      return r;
    }

/* PatternPatternMatch */
    public R visit(rholang.parsing.rholang2.Absyn.PtBranch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_1.accept(this, arg), r, arg);
      r = combine(p.ppattern_2.accept(this, arg), r, arg);
      return r;
    }

/* ValPattern */
    public R visit(rholang.parsing.rholang2.Absyn.VPtStruct p, A arg) {
      R r = leaf(arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }


}
