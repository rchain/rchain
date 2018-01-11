package rholang.parsing.rholang2;
import rholang.parsing.rholang2.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  rholang.parsing.rholang2.Absyn.Contr.Visitor<rholang.parsing.rholang2.Absyn.Contr,A>,
  rholang.parsing.rholang2.Absyn.Proc.Visitor<rholang.parsing.rholang2.Absyn.Proc,A>,
  rholang.parsing.rholang2.Absyn.Chan.Visitor<rholang.parsing.rholang2.Absyn.Chan,A>,
  rholang.parsing.rholang2.Absyn.Bind.Visitor<rholang.parsing.rholang2.Absyn.Bind,A>,
  rholang.parsing.rholang2.Absyn.PMBranch.Visitor<rholang.parsing.rholang2.Absyn.PMBranch,A>,
  rholang.parsing.rholang2.Absyn.CBranch.Visitor<rholang.parsing.rholang2.Absyn.CBranch,A>,
  rholang.parsing.rholang2.Absyn.Value.Visitor<rholang.parsing.rholang2.Absyn.Value,A>,
  rholang.parsing.rholang2.Absyn.Quantity.Visitor<rholang.parsing.rholang2.Absyn.Quantity,A>,
  rholang.parsing.rholang2.Absyn.Entity.Visitor<rholang.parsing.rholang2.Absyn.Entity,A>,
  rholang.parsing.rholang2.Absyn.Struct.Visitor<rholang.parsing.rholang2.Absyn.Struct,A>,
  rholang.parsing.rholang2.Absyn.Collect.Visitor<rholang.parsing.rholang2.Absyn.Collect,A>,
  rholang.parsing.rholang2.Absyn.VarPattern.Visitor<rholang.parsing.rholang2.Absyn.VarPattern,A>,
  rholang.parsing.rholang2.Absyn.PPattern.Visitor<rholang.parsing.rholang2.Absyn.PPattern,A>,
  rholang.parsing.rholang2.Absyn.CPattern.Visitor<rholang.parsing.rholang2.Absyn.CPattern,A>,
  rholang.parsing.rholang2.Absyn.PatternBind.Visitor<rholang.parsing.rholang2.Absyn.PatternBind,A>,
  rholang.parsing.rholang2.Absyn.PatternPatternMatch.Visitor<rholang.parsing.rholang2.Absyn.PatternPatternMatch,A>,
  rholang.parsing.rholang2.Absyn.ValPattern.Visitor<rholang.parsing.rholang2.Absyn.ValPattern,A>
{
/* Contr */
    public Contr visit(rholang.parsing.rholang2.Absyn.DContr p, A arg)
    {
      String name_ = p.name_;
      ListCPattern listcpattern_ = new ListCPattern();
      for (CPattern x : p.listcpattern_)
      {
        listcpattern_.add(x.accept(this,arg));
      }
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.DContr(name_, listcpattern_, proc_);
    }
/* Proc */
    public Proc visit(rholang.parsing.rholang2.Absyn.PNil p, A arg)
    {
      return new rholang.parsing.rholang2.Absyn.PNil();
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PValue p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PValue(value_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.rholang2.Absyn.PVar(var_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PDrop p, A arg)
    {
      Chan chan_ = p.chan_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PDrop(chan_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PInject p, A arg)
    {
      Chan chan_ = p.chan_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PInject(chan_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PLift p, A arg)
    {
      Chan chan_ = p.chan_.accept(this, arg);
      ListProc listproc_ = new ListProc();
      for (Proc x : p.listproc_)
      {
        listproc_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PLift(chan_, listproc_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PInput p, A arg)
    {
      ListBind listbind_ = new ListBind();
      for (Bind x : p.listbind_)
      {
        listbind_.add(x.accept(this,arg));
      }
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PInput(listbind_, proc_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PChoice p, A arg)
    {
      ListCBranch listcbranch_ = new ListCBranch();
      for (CBranch x : p.listcbranch_)
      {
        listcbranch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PChoice(listcbranch_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PMatch p, A arg)
    {
      Proc proc_ = p.proc_.accept(this, arg);
      ListPMBranch listpmbranch_ = new ListPMBranch();
      for (PMBranch x : p.listpmbranch_)
      {
        listpmbranch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PMatch(proc_, listpmbranch_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PNew p, A arg)
    {
      ListVar listvar_ = p.listvar_;
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PNew(listvar_, proc_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PConstr p, A arg)
    {
      String name_ = p.name_;
      ListProc listproc_ = new ListProc();
      for (Proc x : p.listproc_)
      {
        listproc_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PConstr(name_, listproc_);
    }    public Proc visit(rholang.parsing.rholang2.Absyn.PPar p, A arg)
    {
      Proc proc_1 = p.proc_1.accept(this, arg);
      Proc proc_2 = p.proc_2.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPar(proc_1, proc_2);
    }
/* Chan */
    public Chan visit(rholang.parsing.rholang2.Absyn.CVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.rholang2.Absyn.CVar(var_);
    }    public Chan visit(rholang.parsing.rholang2.Absyn.CQuote p, A arg)
    {
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.CQuote(proc_);
    }
/* Bind */
    public Bind visit(rholang.parsing.rholang2.Absyn.InputBind p, A arg)
    {
      CPattern cpattern_ = p.cpattern_.accept(this, arg);
      Chan chan_ = p.chan_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.InputBind(cpattern_, chan_);
    }
/* PMBranch */
    public PMBranch visit(rholang.parsing.rholang2.Absyn.PatternMatch p, A arg)
    {
      PPattern ppattern_ = p.ppattern_.accept(this, arg);
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PatternMatch(ppattern_, proc_);
    }
/* CBranch */
    public CBranch visit(rholang.parsing.rholang2.Absyn.Choice p, A arg)
    {
      ListBind listbind_ = new ListBind();
      for (Bind x : p.listbind_)
      {
        listbind_.add(x.accept(this,arg));
      }
      Proc proc_ = p.proc_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.Choice(listbind_, proc_);
    }
/* Value */
    public Value visit(rholang.parsing.rholang2.Absyn.VQuant p, A arg)
    {
      Quantity quantity_ = p.quantity_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.VQuant(quantity_);
    }    public Value visit(rholang.parsing.rholang2.Absyn.VEnt p, A arg)
    {
      Entity entity_ = p.entity_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.VEnt(entity_);
    }
/* Quantity */
    public Quantity visit(rholang.parsing.rholang2.Absyn.QInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new rholang.parsing.rholang2.Absyn.QInt(integer_);
    }    public Quantity visit(rholang.parsing.rholang2.Absyn.QDouble p, A arg)
    {
      Double double_ = p.double_;
      return new rholang.parsing.rholang2.Absyn.QDouble(double_);
    }
/* Entity */
    public Entity visit(rholang.parsing.rholang2.Absyn.EChar p, A arg)
    {
      Character char_ = p.char_;
      return new rholang.parsing.rholang2.Absyn.EChar(char_);
    }    public Entity visit(rholang.parsing.rholang2.Absyn.EStruct p, A arg)
    {
      Struct struct_ = p.struct_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.EStruct(struct_);
    }    public Entity visit(rholang.parsing.rholang2.Absyn.ECollect p, A arg)
    {
      Collect collect_ = p.collect_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.ECollect(collect_);
    }
/* Struct */
    public Struct visit(rholang.parsing.rholang2.Absyn.StructConstr p, A arg)
    {
      String var_ = p.var_;
      ListProc listproc_ = new ListProc();
      for (Proc x : p.listproc_)
      {
        listproc_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.StructConstr(var_, listproc_);
    }
/* Collect */
    public Collect visit(rholang.parsing.rholang2.Absyn.CString p, A arg)
    {
      String string_ = p.string_;
      return new rholang.parsing.rholang2.Absyn.CString(string_);
    }
/* VarPattern */
    public VarPattern visit(rholang.parsing.rholang2.Absyn.VarPtVar p, A arg)
    {
      String var_ = p.var_;
      return new rholang.parsing.rholang2.Absyn.VarPtVar(var_);
    }    public VarPattern visit(rholang.parsing.rholang2.Absyn.VarPtWild p, A arg)
    {
      return new rholang.parsing.rholang2.Absyn.VarPtWild();
    }
/* PPattern */
    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtVar p, A arg)
    {
      VarPattern varpattern_ = p.varpattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtVar(varpattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtNil p, A arg)
    {
      return new rholang.parsing.rholang2.Absyn.PPtNil();
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtVal p, A arg)
    {
      ValPattern valpattern_ = p.valpattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtVal(valpattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtDrop p, A arg)
    {
      CPattern cpattern_ = p.cpattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtDrop(cpattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtInject p, A arg)
    {
      CPattern cpattern_ = p.cpattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtInject(cpattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtOutput p, A arg)
    {
      CPattern cpattern_ = p.cpattern_.accept(this, arg);
      ListPPattern listppattern_ = new ListPPattern();
      for (PPattern x : p.listppattern_)
      {
        listppattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PPtOutput(cpattern_, listppattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtInput p, A arg)
    {
      ListPatternBind listpatternbind_ = new ListPatternBind();
      for (PatternBind x : p.listpatternbind_)
      {
        listpatternbind_.add(x.accept(this,arg));
      }
      PPattern ppattern_ = p.ppattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtInput(listpatternbind_, ppattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtMatch p, A arg)
    {
      PPattern ppattern_ = p.ppattern_.accept(this, arg);
      ListPatternPatternMatch listpatternpatternmatch_ = new ListPatternPatternMatch();
      for (PatternPatternMatch x : p.listpatternpatternmatch_)
      {
        listpatternpatternmatch_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PPtMatch(ppattern_, listpatternpatternmatch_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtNew p, A arg)
    {
      ListVarPattern listvarpattern_ = new ListVarPattern();
      for (VarPattern x : p.listvarpattern_)
      {
        listvarpattern_.add(x.accept(this,arg));
      }
      PPattern ppattern_ = p.ppattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtNew(listvarpattern_, ppattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtConstr p, A arg)
    {
      String name_ = p.name_;
      ListPPattern listppattern_ = new ListPPattern();
      for (PPattern x : p.listppattern_)
      {
        listppattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.PPtConstr(name_, listppattern_);
    }    public PPattern visit(rholang.parsing.rholang2.Absyn.PPtPar p, A arg)
    {
      PPattern ppattern_1 = p.ppattern_1.accept(this, arg);
      PPattern ppattern_2 = p.ppattern_2.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PPtPar(ppattern_1, ppattern_2);
    }
/* CPattern */
    public CPattern visit(rholang.parsing.rholang2.Absyn.CPtVar p, A arg)
    {
      VarPattern varpattern_ = p.varpattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.CPtVar(varpattern_);
    }    public CPattern visit(rholang.parsing.rholang2.Absyn.CPtQuote p, A arg)
    {
      PPattern ppattern_ = p.ppattern_.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.CPtQuote(ppattern_);
    }
/* PatternBind */
    public PatternBind visit(rholang.parsing.rholang2.Absyn.PtBind p, A arg)
    {
      CPattern cpattern_1 = p.cpattern_1.accept(this, arg);
      CPattern cpattern_2 = p.cpattern_2.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PtBind(cpattern_1, cpattern_2);
    }
/* PatternPatternMatch */
    public PatternPatternMatch visit(rholang.parsing.rholang2.Absyn.PtBranch p, A arg)
    {
      PPattern ppattern_1 = p.ppattern_1.accept(this, arg);
      PPattern ppattern_2 = p.ppattern_2.accept(this, arg);
      return new rholang.parsing.rholang2.Absyn.PtBranch(ppattern_1, ppattern_2);
    }
/* ValPattern */
    public ValPattern visit(rholang.parsing.rholang2.Absyn.VPtStruct p, A arg)
    {
      String var_ = p.var_;
      ListPPattern listppattern_ = new ListPPattern();
      for (PPattern x : p.listppattern_)
      {
        listppattern_.add(x.accept(this,arg));
      }
      return new rholang.parsing.rholang2.Absyn.VPtStruct(var_, listppattern_);
    }
}