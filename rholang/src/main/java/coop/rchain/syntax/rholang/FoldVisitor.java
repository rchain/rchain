package coop.rchain.syntax.rholang;

import coop.rchain.syntax.rholang.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Contr */
    public R visit(coop.rchain.syntax.rholang.Absyn.DContr p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* Proc */
    public R visit(coop.rchain.syntax.rholang.Absyn.PNil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PValue p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PLift p, A arg) {
      R r = leaf(arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PFoldL p, A arg) {
      R r = leaf(arg);
      r = combine(p.bind_1.accept(this, arg), r, arg);
      r = combine(p.bind_2.accept(this, arg), r, arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PFoldR p, A arg) {
      R r = leaf(arg);
      r = combine(p.bind_1.accept(this, arg), r, arg);
      r = combine(p.bind_2.accept(this, arg), r, arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PInput p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PChoice p, A arg) {
      R r = leaf(arg);
      for (CBranch x : p.listcbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      for (PMBranch x : p.listpmbranch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PNew p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPrint p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PConstr p, A arg) {
      R r = leaf(arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PContr p, A arg) {
      R r = leaf(arg);
      for (CPattern x : p.listcpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_1.accept(this, arg), r, arg);
      r = combine(p.proc_2.accept(this, arg), r, arg);
      return r;
    }

/* Chan */
    public R visit(coop.rchain.syntax.rholang.Absyn.CVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.CQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* Bind */
    public R visit(coop.rchain.syntax.rholang.Absyn.InputBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.CondInputBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      r = combine(p.chan_.accept(this, arg), r, arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* PMBranch */
    public R visit(coop.rchain.syntax.rholang.Absyn.PatternMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* CBranch */
    public R visit(coop.rchain.syntax.rholang.Absyn.Choice p, A arg) {
      R r = leaf(arg);
      for (Bind x : p.listbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.proc_.accept(this, arg), r, arg);
      return r;
    }

/* RhoBool */
    public R visit(coop.rchain.syntax.rholang.Absyn.QTrue p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QFalse p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Quantity */
    public R visit(coop.rchain.syntax.rholang.Absyn.QBool p, A arg) {
      R r = leaf(arg);
      r = combine(p.rhobool_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QDouble p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QString p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QMap p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QDot p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_.accept(this, arg), r, arg);
      for (Quantity x : p.listquantity_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QNeg p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QMult p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QDiv p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QAdd p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QMinus p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QLt p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QLte p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QGt p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QGte p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QEq p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.QNeq p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_1.accept(this, arg), r, arg);
      r = combine(p.quantity_2.accept(this, arg), r, arg);
      return r;
    }

/* Value */
    public R visit(coop.rchain.syntax.rholang.Absyn.VQuant p, A arg) {
      R r = leaf(arg);
      r = combine(p.quantity_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.EChar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.ETuple p, A arg) {
      R r = leaf(arg);
      for (Proc x : p.listproc_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }

/* VarPattern */
    public R visit(coop.rchain.syntax.rholang.Absyn.VarPtVar p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VarPtWild p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* PPattern */
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.varpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtNil p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtVal p, A arg) {
      R r = leaf(arg);
      r = combine(p.valpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtDrop p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtInject p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtOutput p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_.accept(this, arg), r, arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtInput p, A arg) {
      R r = leaf(arg);
      for (PatternBind x : p.listpatternbind_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtMatch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      for (PatternPatternMatch x : p.listpatternpatternmatch_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtNew p, A arg) {
      R r = leaf(arg);
      for (VarPattern x : p.listvarpattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtConstr p, A arg) {
      R r = leaf(arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.PPtPar p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_1.accept(this, arg), r, arg);
      r = combine(p.ppattern_2.accept(this, arg), r, arg);
      return r;
    }

/* CPattern */
    public R visit(coop.rchain.syntax.rholang.Absyn.CPtVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.varpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.CValPtrn p, A arg) {
      R r = leaf(arg);
      r = combine(p.valpattern_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.CPtQuote p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_.accept(this, arg), r, arg);
      return r;
    }

/* PatternBind */
    public R visit(coop.rchain.syntax.rholang.Absyn.PtBind p, A arg) {
      R r = leaf(arg);
      r = combine(p.cpattern_1.accept(this, arg), r, arg);
      r = combine(p.cpattern_2.accept(this, arg), r, arg);
      return r;
    }

/* PatternPatternMatch */
    public R visit(coop.rchain.syntax.rholang.Absyn.PtBranch p, A arg) {
      R r = leaf(arg);
      r = combine(p.ppattern_1.accept(this, arg), r, arg);
      r = combine(p.ppattern_2.accept(this, arg), r, arg);
      return r;
    }

/* ValPattern */
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStruct p, A arg) {
      R r = leaf(arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTuple p, A arg) {
      R r = leaf(arg);
      for (PPattern x : p.listppattern_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtTrue p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtFalse p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtDbl p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(coop.rchain.syntax.rholang.Absyn.VPtStr p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
