package rholang.parsing.rholang2;
import rholang.parsing.rholang2.Absyn.*;

public class PrettyPrinter
{
  //For certain applications increasing the initial size of the buffer may improve performance.
  private static final int INITIAL_BUFFER_SIZE = 128;
  private static final int INDENT_WIDTH = 2;
  //You may wish to change the parentheses used in precedence.
  private static final String _L_PAREN = new String("(");
  private static final String _R_PAREN = new String(")");
  //You may wish to change render
  private static void render(String s)
  {
    if (s.equals("{"))
    {
       buf_.append("\n");
       indent();
       buf_.append(s);
       _n_ = _n_ + INDENT_WIDTH;
       buf_.append("\n");
       indent();
    }
    else if (s.equals("(") || s.equals("["))
       buf_.append(s);
    else if (s.equals(")") || s.equals("]"))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals("}"))
    {
       int t;
       _n_ = _n_ - INDENT_WIDTH;
       for(t=0; t<INDENT_WIDTH; t++) {
         backup();
       }
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals(","))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals(";"))
    {
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals("")) return;
    else
    {
       buf_.append(s);
       buf_.append(" ");
    }
  }


  //  print and show methods are defined for each category.
  public static String print(rholang.parsing.rholang2.Absyn.Contr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Contr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Proc foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Proc foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListProc foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListProc foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Chan foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Chan foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Bind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Bind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.PMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.PMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListPMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListPMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.CBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.CBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListCBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListCBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Quantity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Quantity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Entity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Entity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Struct foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Struct foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.Collect foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.Collect foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.VarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.VarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListVarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListVarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.PPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.PPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListPPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListPPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.CPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.CPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListCPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListCPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.PatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.PatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListPatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListPatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.PatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.PatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListPatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListPatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ValPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ValPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListVar foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListVar foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang2.Absyn.ListName foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang2.Absyn.ListName foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(rholang.parsing.rholang2.Absyn.Contr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.DContr)
    {
       rholang.parsing.rholang2.Absyn.DContr _dcontr = (rholang.parsing.rholang2.Absyn.DContr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("contract");
       pp(_dcontr.name_, 0);
       render("(");
       pp(_dcontr.listcpattern_, 0);
       render(")");
       render("=");
       render("{");
       pp(_dcontr.proc_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Proc foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PNil)
    {
       rholang.parsing.rholang2.Absyn.PNil _pnil = (rholang.parsing.rholang2.Absyn.PNil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PValue)
    {
       rholang.parsing.rholang2.Absyn.PValue _pvalue = (rholang.parsing.rholang2.Absyn.PValue) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pvalue.value_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PVar)
    {
       rholang.parsing.rholang2.Absyn.PVar _pvar = (rholang.parsing.rholang2.Absyn.PVar) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pvar.var_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PDrop)
    {
       rholang.parsing.rholang2.Absyn.PDrop _pdrop = (rholang.parsing.rholang2.Absyn.PDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_pdrop.chan_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PInject)
    {
       rholang.parsing.rholang2.Absyn.PInject _pinject = (rholang.parsing.rholang2.Absyn.PInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_pinject.chan_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PLift)
    {
       rholang.parsing.rholang2.Absyn.PLift _plift = (rholang.parsing.rholang2.Absyn.PLift) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_plift.chan_, 0);
       render("!");
       render("(");
       pp(_plift.listproc_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PInput)
    {
       rholang.parsing.rholang2.Absyn.PInput _pinput = (rholang.parsing.rholang2.Absyn.PInput) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("for");
       render("(");
       pp(_pinput.listbind_, 0);
       render(")");
       render("{");
       pp(_pinput.proc_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PChoice)
    {
       rholang.parsing.rholang2.Absyn.PChoice _pchoice = (rholang.parsing.rholang2.Absyn.PChoice) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("select");
       render("{");
       pp(_pchoice.listcbranch_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PMatch)
    {
       rholang.parsing.rholang2.Absyn.PMatch _pmatch = (rholang.parsing.rholang2.Absyn.PMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_pmatch.proc_, 0);
       render("with");
       pp(_pmatch.listpmbranch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PNew)
    {
       rholang.parsing.rholang2.Absyn.PNew _pnew = (rholang.parsing.rholang2.Absyn.PNew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_pnew.listvar_, 0);
       render("in");
       pp(_pnew.proc_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PConstr)
    {
       rholang.parsing.rholang2.Absyn.PConstr _pconstr = (rholang.parsing.rholang2.Absyn.PConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_pconstr.name_, 0);
       render("(");
       pp(_pconstr.listproc_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPar)
    {
       rholang.parsing.rholang2.Absyn.PPar _ppar = (rholang.parsing.rholang2.Absyn.PPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ppar.proc_1, 0);
       render("|");
       pp(_ppar.proc_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListProc foo, int _i_)
  {
     for (java.util.Iterator<Proc> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.Chan foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CVar)
    {
       rholang.parsing.rholang2.Absyn.CVar _cvar = (rholang.parsing.rholang2.Absyn.CVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cvar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.CQuote)
    {
       rholang.parsing.rholang2.Absyn.CQuote _cquote = (rholang.parsing.rholang2.Absyn.CQuote) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       pp(_cquote.proc_, 3);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Bind foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.InputBind)
    {
       rholang.parsing.rholang2.Absyn.InputBind _inputbind = (rholang.parsing.rholang2.Absyn.InputBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_inputbind.cpattern_, 0);
       render("<-");
       pp(_inputbind.chan_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListBind foo, int _i_)
  {
     for (java.util.Iterator<Bind> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(";");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.PMBranch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PatternMatch)
    {
       rholang.parsing.rholang2.Absyn.PatternMatch _patternmatch = (rholang.parsing.rholang2.Absyn.PatternMatch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_patternmatch.ppattern_, 0);
       render("=>");
       render("{");
       pp(_patternmatch.proc_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListPMBranch foo, int _i_)
  {
     for (java.util.Iterator<PMBranch> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.CBranch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.Choice)
    {
       rholang.parsing.rholang2.Absyn.Choice _choice = (rholang.parsing.rholang2.Absyn.Choice) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("case");
       pp(_choice.listbind_, 0);
       render("=>");
       render("{");
       pp(_choice.proc_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListCBranch foo, int _i_)
  {
     for (java.util.Iterator<CBranch> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.Value foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VQuant)
    {
       rholang.parsing.rholang2.Absyn.VQuant _vquant = (rholang.parsing.rholang2.Absyn.VQuant) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vquant.quantity_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.VEnt)
    {
       rholang.parsing.rholang2.Absyn.VEnt _vent = (rholang.parsing.rholang2.Absyn.VEnt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vent.entity_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Quantity foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.QInt)
    {
       rholang.parsing.rholang2.Absyn.QInt _qint = (rholang.parsing.rholang2.Absyn.QInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.QDouble)
    {
       rholang.parsing.rholang2.Absyn.QDouble _qdouble = (rholang.parsing.rholang2.Absyn.QDouble) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qdouble.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Entity foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.EChar)
    {
       rholang.parsing.rholang2.Absyn.EChar _echar = (rholang.parsing.rholang2.Absyn.EChar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_echar.char_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.EStruct)
    {
       rholang.parsing.rholang2.Absyn.EStruct _estruct = (rholang.parsing.rholang2.Absyn.EStruct) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_estruct.struct_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.ECollect)
    {
       rholang.parsing.rholang2.Absyn.ECollect _ecollect = (rholang.parsing.rholang2.Absyn.ECollect) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ecollect.collect_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Struct foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.StructConstr)
    {
       rholang.parsing.rholang2.Absyn.StructConstr _structconstr = (rholang.parsing.rholang2.Absyn.StructConstr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_structconstr.var_, 0);
       render("{");
       pp(_structconstr.listproc_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.Collect foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CString)
    {
       rholang.parsing.rholang2.Absyn.CString _cstring = (rholang.parsing.rholang2.Absyn.CString) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cstring.string_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.VarPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VarPtVar)
    {
       rholang.parsing.rholang2.Absyn.VarPtVar _varptvar = (rholang.parsing.rholang2.Absyn.VarPtVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_varptvar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.VarPtWild)
    {
       rholang.parsing.rholang2.Absyn.VarPtWild _varptwild = (rholang.parsing.rholang2.Absyn.VarPtWild) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListVarPattern foo, int _i_)
  {
     for (java.util.Iterator<VarPattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.PPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtVar)
    {
       rholang.parsing.rholang2.Absyn.PPtVar _pptvar = (rholang.parsing.rholang2.Absyn.PPtVar) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pptvar.varpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtNil)
    {
       rholang.parsing.rholang2.Absyn.PPtNil _pptnil = (rholang.parsing.rholang2.Absyn.PPtNil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtVal)
    {
       rholang.parsing.rholang2.Absyn.PPtVal _pptval = (rholang.parsing.rholang2.Absyn.PPtVal) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pptval.valpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtDrop)
    {
       rholang.parsing.rholang2.Absyn.PPtDrop _pptdrop = (rholang.parsing.rholang2.Absyn.PPtDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_pptdrop.cpattern_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtInject)
    {
       rholang.parsing.rholang2.Absyn.PPtInject _pptinject = (rholang.parsing.rholang2.Absyn.PPtInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_pptinject.cpattern_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtOutput)
    {
       rholang.parsing.rholang2.Absyn.PPtOutput _pptoutput = (rholang.parsing.rholang2.Absyn.PPtOutput) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_pptoutput.cpattern_, 0);
       render("!");
       render("(");
       pp(_pptoutput.listppattern_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtInput)
    {
       rholang.parsing.rholang2.Absyn.PPtInput _pptinput = (rholang.parsing.rholang2.Absyn.PPtInput) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("for");
       render("(");
       pp(_pptinput.listpatternbind_, 0);
       render(")");
       render("{");
       pp(_pptinput.ppattern_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtMatch)
    {
       rholang.parsing.rholang2.Absyn.PPtMatch _pptmatch = (rholang.parsing.rholang2.Absyn.PPtMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_pptmatch.ppattern_, 0);
       render("with");
       pp(_pptmatch.listpatternpatternmatch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtNew)
    {
       rholang.parsing.rholang2.Absyn.PPtNew _pptnew = (rholang.parsing.rholang2.Absyn.PPtNew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_pptnew.listvarpattern_, 0);
       render("in");
       pp(_pptnew.ppattern_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtConstr)
    {
       rholang.parsing.rholang2.Absyn.PPtConstr _pptconstr = (rholang.parsing.rholang2.Absyn.PPtConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_pptconstr.name_, 0);
       render("(");
       pp(_pptconstr.listppattern_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.PPtPar)
    {
       rholang.parsing.rholang2.Absyn.PPtPar _pptpar = (rholang.parsing.rholang2.Absyn.PPtPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pptpar.ppattern_1, 0);
       render("|");
       pp(_pptpar.ppattern_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListPPattern foo, int _i_)
  {
     for (java.util.Iterator<PPattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.CPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CPtVar)
    {
       rholang.parsing.rholang2.Absyn.CPtVar _cptvar = (rholang.parsing.rholang2.Absyn.CPtVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cptvar.varpattern_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang2.Absyn.CPtQuote)
    {
       rholang.parsing.rholang2.Absyn.CPtQuote _cptquote = (rholang.parsing.rholang2.Absyn.CPtQuote) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       pp(_cptquote.ppattern_, 3);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListCPattern foo, int _i_)
  {
     for (java.util.Iterator<CPattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.PatternBind foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PtBind)
    {
       rholang.parsing.rholang2.Absyn.PtBind _ptbind = (rholang.parsing.rholang2.Absyn.PtBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbind.cpattern_1, 0);
       render("<-");
       pp(_ptbind.cpattern_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListPatternBind foo, int _i_)
  {
     for (java.util.Iterator<PatternBind> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(";");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.PatternPatternMatch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PtBranch)
    {
       rholang.parsing.rholang2.Absyn.PtBranch _ptbranch = (rholang.parsing.rholang2.Absyn.PtBranch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbranch.ppattern_1, 0);
       render("=>");
       render("{");
       pp(_ptbranch.ppattern_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListPatternPatternMatch foo, int _i_)
  {
     for (java.util.Iterator<PatternPatternMatch> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render("");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.ValPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VPtStruct)
    {
       rholang.parsing.rholang2.Absyn.VPtStruct _vptstruct = (rholang.parsing.rholang2.Absyn.VPtStruct) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vptstruct.var_, 0);
       render("{");
       pp(_vptstruct.listppattern_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListVar foo, int _i_)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang2.Absyn.ListName foo, int _i_)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }


  private static void sh(rholang.parsing.rholang2.Absyn.Contr foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.DContr)
    {
       rholang.parsing.rholang2.Absyn.DContr _dcontr = (rholang.parsing.rholang2.Absyn.DContr) foo;
       render("(");
       render("DContr");
       sh(_dcontr.name_);
       render("[");
       sh(_dcontr.listcpattern_);
       render("]");
       sh(_dcontr.proc_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Proc foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PNil)
    {
       rholang.parsing.rholang2.Absyn.PNil _pnil = (rholang.parsing.rholang2.Absyn.PNil) foo;
       render("PNil");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PValue)
    {
       rholang.parsing.rholang2.Absyn.PValue _pvalue = (rholang.parsing.rholang2.Absyn.PValue) foo;
       render("(");
       render("PValue");
       sh(_pvalue.value_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PVar)
    {
       rholang.parsing.rholang2.Absyn.PVar _pvar = (rholang.parsing.rholang2.Absyn.PVar) foo;
       render("(");
       render("PVar");
       sh(_pvar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PDrop)
    {
       rholang.parsing.rholang2.Absyn.PDrop _pdrop = (rholang.parsing.rholang2.Absyn.PDrop) foo;
       render("(");
       render("PDrop");
       sh(_pdrop.chan_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PInject)
    {
       rholang.parsing.rholang2.Absyn.PInject _pinject = (rholang.parsing.rholang2.Absyn.PInject) foo;
       render("(");
       render("PInject");
       sh(_pinject.chan_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PLift)
    {
       rholang.parsing.rholang2.Absyn.PLift _plift = (rholang.parsing.rholang2.Absyn.PLift) foo;
       render("(");
       render("PLift");
       sh(_plift.chan_);
       render("[");
       sh(_plift.listproc_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PInput)
    {
       rholang.parsing.rholang2.Absyn.PInput _pinput = (rholang.parsing.rholang2.Absyn.PInput) foo;
       render("(");
       render("PInput");
       render("[");
       sh(_pinput.listbind_);
       render("]");
       sh(_pinput.proc_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PChoice)
    {
       rholang.parsing.rholang2.Absyn.PChoice _pchoice = (rholang.parsing.rholang2.Absyn.PChoice) foo;
       render("(");
       render("PChoice");
       render("[");
       sh(_pchoice.listcbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PMatch)
    {
       rholang.parsing.rholang2.Absyn.PMatch _pmatch = (rholang.parsing.rholang2.Absyn.PMatch) foo;
       render("(");
       render("PMatch");
       sh(_pmatch.proc_);
       render("[");
       sh(_pmatch.listpmbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PNew)
    {
       rholang.parsing.rholang2.Absyn.PNew _pnew = (rholang.parsing.rholang2.Absyn.PNew) foo;
       render("(");
       render("PNew");
       render("[");
       sh(_pnew.listvar_);
       render("]");
       sh(_pnew.proc_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PConstr)
    {
       rholang.parsing.rholang2.Absyn.PConstr _pconstr = (rholang.parsing.rholang2.Absyn.PConstr) foo;
       render("(");
       render("PConstr");
       sh(_pconstr.name_);
       render("[");
       sh(_pconstr.listproc_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPar)
    {
       rholang.parsing.rholang2.Absyn.PPar _ppar = (rholang.parsing.rholang2.Absyn.PPar) foo;
       render("(");
       render("PPar");
       sh(_ppar.proc_1);
       sh(_ppar.proc_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListProc foo)
  {
     for (java.util.Iterator<Proc> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Chan foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CVar)
    {
       rholang.parsing.rholang2.Absyn.CVar _cvar = (rholang.parsing.rholang2.Absyn.CVar) foo;
       render("(");
       render("CVar");
       sh(_cvar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.CQuote)
    {
       rholang.parsing.rholang2.Absyn.CQuote _cquote = (rholang.parsing.rholang2.Absyn.CQuote) foo;
       render("(");
       render("CQuote");
       sh(_cquote.proc_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Bind foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.InputBind)
    {
       rholang.parsing.rholang2.Absyn.InputBind _inputbind = (rholang.parsing.rholang2.Absyn.InputBind) foo;
       render("(");
       render("InputBind");
       sh(_inputbind.cpattern_);
       sh(_inputbind.chan_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListBind foo)
  {
     for (java.util.Iterator<Bind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.PMBranch foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PatternMatch)
    {
       rholang.parsing.rholang2.Absyn.PatternMatch _patternmatch = (rholang.parsing.rholang2.Absyn.PatternMatch) foo;
       render("(");
       render("PatternMatch");
       sh(_patternmatch.ppattern_);
       sh(_patternmatch.proc_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListPMBranch foo)
  {
     for (java.util.Iterator<PMBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.CBranch foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.Choice)
    {
       rholang.parsing.rholang2.Absyn.Choice _choice = (rholang.parsing.rholang2.Absyn.Choice) foo;
       render("(");
       render("Choice");
       render("[");
       sh(_choice.listbind_);
       render("]");
       sh(_choice.proc_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListCBranch foo)
  {
     for (java.util.Iterator<CBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Value foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VQuant)
    {
       rholang.parsing.rholang2.Absyn.VQuant _vquant = (rholang.parsing.rholang2.Absyn.VQuant) foo;
       render("(");
       render("VQuant");
       sh(_vquant.quantity_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.VEnt)
    {
       rholang.parsing.rholang2.Absyn.VEnt _vent = (rholang.parsing.rholang2.Absyn.VEnt) foo;
       render("(");
       render("VEnt");
       sh(_vent.entity_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Quantity foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.QInt)
    {
       rholang.parsing.rholang2.Absyn.QInt _qint = (rholang.parsing.rholang2.Absyn.QInt) foo;
       render("(");
       render("QInt");
       sh(_qint.integer_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.QDouble)
    {
       rholang.parsing.rholang2.Absyn.QDouble _qdouble = (rholang.parsing.rholang2.Absyn.QDouble) foo;
       render("(");
       render("QDouble");
       sh(_qdouble.double_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Entity foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.EChar)
    {
       rholang.parsing.rholang2.Absyn.EChar _echar = (rholang.parsing.rholang2.Absyn.EChar) foo;
       render("(");
       render("EChar");
       sh(_echar.char_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.EStruct)
    {
       rholang.parsing.rholang2.Absyn.EStruct _estruct = (rholang.parsing.rholang2.Absyn.EStruct) foo;
       render("(");
       render("EStruct");
       sh(_estruct.struct_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.ECollect)
    {
       rholang.parsing.rholang2.Absyn.ECollect _ecollect = (rholang.parsing.rholang2.Absyn.ECollect) foo;
       render("(");
       render("ECollect");
       sh(_ecollect.collect_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Struct foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.StructConstr)
    {
       rholang.parsing.rholang2.Absyn.StructConstr _structconstr = (rholang.parsing.rholang2.Absyn.StructConstr) foo;
       render("(");
       render("StructConstr");
       sh(_structconstr.var_);
       render("[");
       sh(_structconstr.listproc_);
       render("]");
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.Collect foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CString)
    {
       rholang.parsing.rholang2.Absyn.CString _cstring = (rholang.parsing.rholang2.Absyn.CString) foo;
       render("(");
       render("CString");
       sh(_cstring.string_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.VarPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VarPtVar)
    {
       rholang.parsing.rholang2.Absyn.VarPtVar _varptvar = (rholang.parsing.rholang2.Absyn.VarPtVar) foo;
       render("(");
       render("VarPtVar");
       sh(_varptvar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.VarPtWild)
    {
       rholang.parsing.rholang2.Absyn.VarPtWild _varptwild = (rholang.parsing.rholang2.Absyn.VarPtWild) foo;
       render("VarPtWild");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListVarPattern foo)
  {
     for (java.util.Iterator<VarPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.PPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtVar)
    {
       rholang.parsing.rholang2.Absyn.PPtVar _pptvar = (rholang.parsing.rholang2.Absyn.PPtVar) foo;
       render("(");
       render("PPtVar");
       sh(_pptvar.varpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtNil)
    {
       rholang.parsing.rholang2.Absyn.PPtNil _pptnil = (rholang.parsing.rholang2.Absyn.PPtNil) foo;
       render("PPtNil");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtVal)
    {
       rholang.parsing.rholang2.Absyn.PPtVal _pptval = (rholang.parsing.rholang2.Absyn.PPtVal) foo;
       render("(");
       render("PPtVal");
       sh(_pptval.valpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtDrop)
    {
       rholang.parsing.rholang2.Absyn.PPtDrop _pptdrop = (rholang.parsing.rholang2.Absyn.PPtDrop) foo;
       render("(");
       render("PPtDrop");
       sh(_pptdrop.cpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtInject)
    {
       rholang.parsing.rholang2.Absyn.PPtInject _pptinject = (rholang.parsing.rholang2.Absyn.PPtInject) foo;
       render("(");
       render("PPtInject");
       sh(_pptinject.cpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtOutput)
    {
       rholang.parsing.rholang2.Absyn.PPtOutput _pptoutput = (rholang.parsing.rholang2.Absyn.PPtOutput) foo;
       render("(");
       render("PPtOutput");
       sh(_pptoutput.cpattern_);
       render("[");
       sh(_pptoutput.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtInput)
    {
       rholang.parsing.rholang2.Absyn.PPtInput _pptinput = (rholang.parsing.rholang2.Absyn.PPtInput) foo;
       render("(");
       render("PPtInput");
       render("[");
       sh(_pptinput.listpatternbind_);
       render("]");
       sh(_pptinput.ppattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtMatch)
    {
       rholang.parsing.rholang2.Absyn.PPtMatch _pptmatch = (rholang.parsing.rholang2.Absyn.PPtMatch) foo;
       render("(");
       render("PPtMatch");
       sh(_pptmatch.ppattern_);
       render("[");
       sh(_pptmatch.listpatternpatternmatch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtNew)
    {
       rholang.parsing.rholang2.Absyn.PPtNew _pptnew = (rholang.parsing.rholang2.Absyn.PPtNew) foo;
       render("(");
       render("PPtNew");
       render("[");
       sh(_pptnew.listvarpattern_);
       render("]");
       sh(_pptnew.ppattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtConstr)
    {
       rholang.parsing.rholang2.Absyn.PPtConstr _pptconstr = (rholang.parsing.rholang2.Absyn.PPtConstr) foo;
       render("(");
       render("PPtConstr");
       sh(_pptconstr.name_);
       render("[");
       sh(_pptconstr.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.PPtPar)
    {
       rholang.parsing.rholang2.Absyn.PPtPar _pptpar = (rholang.parsing.rholang2.Absyn.PPtPar) foo;
       render("(");
       render("PPtPar");
       sh(_pptpar.ppattern_1);
       sh(_pptpar.ppattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListPPattern foo)
  {
     for (java.util.Iterator<PPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.CPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.CPtVar)
    {
       rholang.parsing.rholang2.Absyn.CPtVar _cptvar = (rholang.parsing.rholang2.Absyn.CPtVar) foo;
       render("(");
       render("CPtVar");
       sh(_cptvar.varpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang2.Absyn.CPtQuote)
    {
       rholang.parsing.rholang2.Absyn.CPtQuote _cptquote = (rholang.parsing.rholang2.Absyn.CPtQuote) foo;
       render("(");
       render("CPtQuote");
       sh(_cptquote.ppattern_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListCPattern foo)
  {
     for (java.util.Iterator<CPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.PatternBind foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PtBind)
    {
       rholang.parsing.rholang2.Absyn.PtBind _ptbind = (rholang.parsing.rholang2.Absyn.PtBind) foo;
       render("(");
       render("PtBind");
       sh(_ptbind.cpattern_1);
       sh(_ptbind.cpattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListPatternBind foo)
  {
     for (java.util.Iterator<PatternBind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.PatternPatternMatch foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.PtBranch)
    {
       rholang.parsing.rholang2.Absyn.PtBranch _ptbranch = (rholang.parsing.rholang2.Absyn.PtBranch) foo;
       render("(");
       render("PtBranch");
       sh(_ptbranch.ppattern_1);
       sh(_ptbranch.ppattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListPatternPatternMatch foo)
  {
     for (java.util.Iterator<PatternPatternMatch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ValPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang2.Absyn.VPtStruct)
    {
       rholang.parsing.rholang2.Absyn.VPtStruct _vptstruct = (rholang.parsing.rholang2.Absyn.VPtStruct) foo;
       render("(");
       render("VPtStruct");
       sh(_vptstruct.var_);
       render("[");
       sh(_vptstruct.listppattern_);
       render("]");
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListVar foo)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang2.Absyn.ListName foo)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }


  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(" "); }
  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(" "); }
  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(" "); }
  private static void pp(Character c, int _i_) { buf_.append("'" + c.toString() + "'"); buf_.append(" "); }
  private static void sh(Integer n) { render(n.toString()); }
  private static void sh(Double d) { render(d.toString()); }
  private static void sh(Character c) { render(c.toString()); }
  private static void sh(String s) { printQuoted(s); }
  private static void printQuoted(String s) { render("\"" + s + "\""); }
  private static void indent()
  {
    int n = _n_;
    while (n > 0)
    {
      buf_.append(" ");
      n--;
    }
  }
  private static void backup()
  {
     if (buf_.charAt(buf_.length() - 1) == ' ') {
      buf_.setLength(buf_.length() - 1);
    }
  }
  private static void trim()
  {
     while (buf_.length() > 0 && buf_.charAt(0) == ' ')
        buf_.deleteCharAt(0); 
    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')
        buf_.deleteCharAt(buf_.length()-1);
  }
  private static int _n_ = 0;
  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);
}

