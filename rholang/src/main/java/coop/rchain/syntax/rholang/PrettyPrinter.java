package coop.rchain.syntax.rholang;
import coop.rchain.syntax.rholang.Absyn.*;

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
  public static String print(coop.rchain.syntax.rholang.Absyn.Contr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Contr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.Proc foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Proc foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListProc foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListProc foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.Chan foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Chan foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.Bind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Bind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.PMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.PMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListPMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListPMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.CBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.CBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListCBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListCBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.RhoBool foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.RhoBool foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.Quantity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Quantity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListQuantity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListQuantity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.VarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.VarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListVarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListVarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.PPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.PPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListPPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListPPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.CPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.CPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListCPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListCPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.PatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.PatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListPatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListPatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.PatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.PatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListPatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListPatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ValPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ValPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListValPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListValPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListVar foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListVar foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(coop.rchain.syntax.rholang.Absyn.ListName foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(coop.rchain.syntax.rholang.Absyn.ListName foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(coop.rchain.syntax.rholang.Absyn.Contr foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.DContr)
    {
       coop.rchain.syntax.rholang.Absyn.DContr _dcontr = (coop.rchain.syntax.rholang.Absyn.DContr) foo;
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.Proc foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PNil)
    {
       coop.rchain.syntax.rholang.Absyn.PNil _pnil = (coop.rchain.syntax.rholang.Absyn.PNil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PValue)
    {
       coop.rchain.syntax.rholang.Absyn.PValue _pvalue = (coop.rchain.syntax.rholang.Absyn.PValue) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pvalue.value_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PDrop)
    {
       coop.rchain.syntax.rholang.Absyn.PDrop _pdrop = (coop.rchain.syntax.rholang.Absyn.PDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_pdrop.chan_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PInject)
    {
       coop.rchain.syntax.rholang.Absyn.PInject _pinject = (coop.rchain.syntax.rholang.Absyn.PInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_pinject.chan_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PLift)
    {
       coop.rchain.syntax.rholang.Absyn.PLift _plift = (coop.rchain.syntax.rholang.Absyn.PLift) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_plift.chan_, 0);
       render("!");
       render("(");
       pp(_plift.listproc_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PFoldL)
    {
       coop.rchain.syntax.rholang.Absyn.PFoldL _pfoldl = (coop.rchain.syntax.rholang.Absyn.PFoldL) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("sum");
       render("(");
       pp(_pfoldl.bind_1, 0);
       render("/:");
       pp(_pfoldl.bind_2, 0);
       render(")");
       render("{");
       pp(_pfoldl.proc_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PFoldR)
    {
       coop.rchain.syntax.rholang.Absyn.PFoldR _pfoldr = (coop.rchain.syntax.rholang.Absyn.PFoldR) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("total");
       render("(");
       pp(_pfoldr.bind_1, 0);
       render(":\\");
       pp(_pfoldr.bind_2, 0);
       render(")");
       render("{");
       pp(_pfoldr.proc_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PInput)
    {
       coop.rchain.syntax.rholang.Absyn.PInput _pinput = (coop.rchain.syntax.rholang.Absyn.PInput) foo;
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
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PChoice)
    {
       coop.rchain.syntax.rholang.Absyn.PChoice _pchoice = (coop.rchain.syntax.rholang.Absyn.PChoice) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("select");
       render("{");
       pp(_pchoice.listcbranch_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PMatch _pmatch = (coop.rchain.syntax.rholang.Absyn.PMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_pmatch.proc_, 0);
       render("with");
       pp(_pmatch.listpmbranch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PNew)
    {
       coop.rchain.syntax.rholang.Absyn.PNew _pnew = (coop.rchain.syntax.rholang.Absyn.PNew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_pnew.listvar_, 0);
       render("in");
       pp(_pnew.proc_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PConstr)
    {
       coop.rchain.syntax.rholang.Absyn.PConstr _pconstr = (coop.rchain.syntax.rholang.Absyn.PConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_pconstr.name_, 0);
       render("(");
       pp(_pconstr.listproc_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPar)
    {
       coop.rchain.syntax.rholang.Absyn.PPar _ppar = (coop.rchain.syntax.rholang.Absyn.PPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ppar.proc_1, 0);
       render("|");
       pp(_ppar.proc_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListProc foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.Chan foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CVar)
    {
       coop.rchain.syntax.rholang.Absyn.CVar _cvar = (coop.rchain.syntax.rholang.Absyn.CVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cvar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.CQuote)
    {
       coop.rchain.syntax.rholang.Absyn.CQuote _cquote = (coop.rchain.syntax.rholang.Absyn.CQuote) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       pp(_cquote.proc_, 3);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.Bind foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.InputBind)
    {
       coop.rchain.syntax.rholang.Absyn.InputBind _inputbind = (coop.rchain.syntax.rholang.Absyn.InputBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_inputbind.cpattern_, 0);
       render("<-");
       pp(_inputbind.chan_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.CondInputBind)
    {
       coop.rchain.syntax.rholang.Absyn.CondInputBind _condinputbind = (coop.rchain.syntax.rholang.Absyn.CondInputBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_condinputbind.cpattern_, 0);
       render("<-");
       pp(_condinputbind.chan_, 0);
       render("if");
       pp(_condinputbind.proc_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListBind foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.PMBranch foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PatternMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PatternMatch _patternmatch = (coop.rchain.syntax.rholang.Absyn.PatternMatch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_patternmatch.ppattern_, 0);
       render("=>");
       render("{");
       pp(_patternmatch.proc_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListPMBranch foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.CBranch foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.Choice)
    {
       coop.rchain.syntax.rholang.Absyn.Choice _choice = (coop.rchain.syntax.rholang.Absyn.Choice) foo;
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListCBranch foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.RhoBool foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QTrue)
    {
       coop.rchain.syntax.rholang.Absyn.QTrue _qtrue = (coop.rchain.syntax.rholang.Absyn.QTrue) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QFalse)
    {
       coop.rchain.syntax.rholang.Absyn.QFalse _qfalse = (coop.rchain.syntax.rholang.Absyn.QFalse) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.Quantity foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QBool)
    {
       coop.rchain.syntax.rholang.Absyn.QBool _qbool = (coop.rchain.syntax.rholang.Absyn.QBool) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_qbool.rhobool_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QInt)
    {
       coop.rchain.syntax.rholang.Absyn.QInt _qint = (coop.rchain.syntax.rholang.Absyn.QInt) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_qint.integer_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDouble)
    {
       coop.rchain.syntax.rholang.Absyn.QDouble _qdouble = (coop.rchain.syntax.rholang.Absyn.QDouble) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_qdouble.double_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QString)
    {
       coop.rchain.syntax.rholang.Absyn.QString _qstring = (coop.rchain.syntax.rholang.Absyn.QString) foo;
       if (_i_ > 7) render(_L_PAREN);
       printQuoted(_qstring.string_);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QVar)
    {
       coop.rchain.syntax.rholang.Absyn.QVar _qvar = (coop.rchain.syntax.rholang.Absyn.QVar) foo;
       if (_i_ > 7) render(_L_PAREN);
       pp(_qvar.var_, 0);
       if (_i_ > 7) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDot)
    {
       coop.rchain.syntax.rholang.Absyn.QDot _qdot = (coop.rchain.syntax.rholang.Absyn.QDot) foo;
       if (_i_ > 6) render(_L_PAREN);
       pp(_qdot.quantity_, 7);
       render(".");
       pp(_qdot.var_, 0);
       render("(");
       pp(_qdot.listquantity_, 0);
       render(")");
       if (_i_ > 6) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QNeg)
    {
       coop.rchain.syntax.rholang.Absyn.QNeg _qneg = (coop.rchain.syntax.rholang.Absyn.QNeg) foo;
       if (_i_ > 5) render(_L_PAREN);
       render("-");
       pp(_qneg.quantity_, 6);
       if (_i_ > 5) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QMult)
    {
       coop.rchain.syntax.rholang.Absyn.QMult _qmult = (coop.rchain.syntax.rholang.Absyn.QMult) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_qmult.quantity_1, 4);
       render("*");
       pp(_qmult.quantity_2, 5);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDiv)
    {
       coop.rchain.syntax.rholang.Absyn.QDiv _qdiv = (coop.rchain.syntax.rholang.Absyn.QDiv) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_qdiv.quantity_1, 4);
       render("/");
       pp(_qdiv.quantity_2, 5);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QAdd)
    {
       coop.rchain.syntax.rholang.Absyn.QAdd _qadd = (coop.rchain.syntax.rholang.Absyn.QAdd) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_qadd.quantity_1, 3);
       render("+");
       pp(_qadd.quantity_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QMinus)
    {
       coop.rchain.syntax.rholang.Absyn.QMinus _qminus = (coop.rchain.syntax.rholang.Absyn.QMinus) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_qminus.quantity_1, 3);
       render("-");
       pp(_qminus.quantity_2, 4);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QLt)
    {
       coop.rchain.syntax.rholang.Absyn.QLt _qlt = (coop.rchain.syntax.rholang.Absyn.QLt) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_qlt.quantity_1, 2);
       render("<");
       pp(_qlt.quantity_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QLte)
    {
       coop.rchain.syntax.rholang.Absyn.QLte _qlte = (coop.rchain.syntax.rholang.Absyn.QLte) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_qlte.quantity_1, 2);
       render("<=");
       pp(_qlte.quantity_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QGt)
    {
       coop.rchain.syntax.rholang.Absyn.QGt _qgt = (coop.rchain.syntax.rholang.Absyn.QGt) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_qgt.quantity_1, 2);
       render(">");
       pp(_qgt.quantity_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QGte)
    {
       coop.rchain.syntax.rholang.Absyn.QGte _qgte = (coop.rchain.syntax.rholang.Absyn.QGte) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_qgte.quantity_1, 2);
       render(">=");
       pp(_qgte.quantity_2, 3);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QEq)
    {
       coop.rchain.syntax.rholang.Absyn.QEq _qeq = (coop.rchain.syntax.rholang.Absyn.QEq) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_qeq.quantity_1, 1);
       render("==");
       pp(_qeq.quantity_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.QNeq)
    {
       coop.rchain.syntax.rholang.Absyn.QNeq _qneq = (coop.rchain.syntax.rholang.Absyn.QNeq) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_qneq.quantity_1, 1);
       render("!=");
       pp(_qneq.quantity_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListQuantity foo, int _i_)
  {
     for (java.util.Iterator<Quantity> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.Value foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VQuant)
    {
       coop.rchain.syntax.rholang.Absyn.VQuant _vquant = (coop.rchain.syntax.rholang.Absyn.VQuant) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vquant.quantity_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.EChar)
    {
       coop.rchain.syntax.rholang.Absyn.EChar _echar = (coop.rchain.syntax.rholang.Absyn.EChar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_echar.char_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.ETuple)
    {
       coop.rchain.syntax.rholang.Absyn.ETuple _etuple = (coop.rchain.syntax.rholang.Absyn.ETuple) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_etuple.listproc_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.VarPattern foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VarPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.VarPtVar _varptvar = (coop.rchain.syntax.rholang.Absyn.VarPtVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_varptvar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VarPtWild)
    {
       coop.rchain.syntax.rholang.Absyn.VarPtWild _varptwild = (coop.rchain.syntax.rholang.Absyn.VarPtWild) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListVarPattern foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.PPattern foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.PPtVar _pptvar = (coop.rchain.syntax.rholang.Absyn.PPtVar) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pptvar.varpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtNil)
    {
       coop.rchain.syntax.rholang.Absyn.PPtNil _pptnil = (coop.rchain.syntax.rholang.Absyn.PPtNil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtVal)
    {
       coop.rchain.syntax.rholang.Absyn.PPtVal _pptval = (coop.rchain.syntax.rholang.Absyn.PPtVal) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_pptval.valpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtDrop)
    {
       coop.rchain.syntax.rholang.Absyn.PPtDrop _pptdrop = (coop.rchain.syntax.rholang.Absyn.PPtDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_pptdrop.cpattern_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtInject)
    {
       coop.rchain.syntax.rholang.Absyn.PPtInject _pptinject = (coop.rchain.syntax.rholang.Absyn.PPtInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_pptinject.cpattern_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtOutput)
    {
       coop.rchain.syntax.rholang.Absyn.PPtOutput _pptoutput = (coop.rchain.syntax.rholang.Absyn.PPtOutput) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_pptoutput.cpattern_, 0);
       render("!");
       render("(");
       pp(_pptoutput.listppattern_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtInput)
    {
       coop.rchain.syntax.rholang.Absyn.PPtInput _pptinput = (coop.rchain.syntax.rholang.Absyn.PPtInput) foo;
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
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PPtMatch _pptmatch = (coop.rchain.syntax.rholang.Absyn.PPtMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_pptmatch.ppattern_, 0);
       render("with");
       pp(_pptmatch.listpatternpatternmatch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtNew)
    {
       coop.rchain.syntax.rholang.Absyn.PPtNew _pptnew = (coop.rchain.syntax.rholang.Absyn.PPtNew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_pptnew.listvarpattern_, 0);
       render("in");
       pp(_pptnew.ppattern_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtConstr)
    {
       coop.rchain.syntax.rholang.Absyn.PPtConstr _pptconstr = (coop.rchain.syntax.rholang.Absyn.PPtConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_pptconstr.name_, 0);
       render("(");
       pp(_pptconstr.listppattern_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtPar)
    {
       coop.rchain.syntax.rholang.Absyn.PPtPar _pptpar = (coop.rchain.syntax.rholang.Absyn.PPtPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pptpar.ppattern_1, 0);
       render("|");
       pp(_pptpar.ppattern_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListPPattern foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.CPattern foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.CPtVar _cptvar = (coop.rchain.syntax.rholang.Absyn.CPtVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cptvar.varpattern_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.CValPtrn)
    {
       coop.rchain.syntax.rholang.Absyn.CValPtrn _cvalptrn = (coop.rchain.syntax.rholang.Absyn.CValPtrn) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cvalptrn.valpattern_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.CPtQuote)
    {
       coop.rchain.syntax.rholang.Absyn.CPtQuote _cptquote = (coop.rchain.syntax.rholang.Absyn.CPtQuote) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       pp(_cptquote.ppattern_, 3);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListCPattern foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.PatternBind foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PtBind)
    {
       coop.rchain.syntax.rholang.Absyn.PtBind _ptbind = (coop.rchain.syntax.rholang.Absyn.PtBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbind.cpattern_1, 0);
       render("<-");
       pp(_ptbind.cpattern_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListPatternBind foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.PatternPatternMatch foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PtBranch)
    {
       coop.rchain.syntax.rholang.Absyn.PtBranch _ptbranch = (coop.rchain.syntax.rholang.Absyn.PtBranch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbranch.ppattern_1, 0);
       render("=>");
       render("{");
       pp(_ptbranch.ppattern_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListPatternPatternMatch foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.ValPattern foo, int _i_)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtStruct)
    {
       coop.rchain.syntax.rholang.Absyn.VPtStruct _vptstruct = (coop.rchain.syntax.rholang.Absyn.VPtStruct) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vptstruct.var_, 0);
       render("{");
       pp(_vptstruct.listppattern_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtTuple)
    {
       coop.rchain.syntax.rholang.Absyn.VPtTuple _vpttuple = (coop.rchain.syntax.rholang.Absyn.VPtTuple) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("<");
       pp(_vpttuple.listppattern_, 0);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtTrue)
    {
       coop.rchain.syntax.rholang.Absyn.VPtTrue _vpttrue = (coop.rchain.syntax.rholang.Absyn.VPtTrue) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtFalse)
    {
       coop.rchain.syntax.rholang.Absyn.VPtFalse _vptfalse = (coop.rchain.syntax.rholang.Absyn.VPtFalse) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtInt)
    {
       coop.rchain.syntax.rholang.Absyn.VPtInt _vptint = (coop.rchain.syntax.rholang.Absyn.VPtInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vptint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtDbl)
    {
       coop.rchain.syntax.rholang.Absyn.VPtDbl _vptdbl = (coop.rchain.syntax.rholang.Absyn.VPtDbl) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vptdbl.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtStr)
    {
       coop.rchain.syntax.rholang.Absyn.VPtStr _vptstr = (coop.rchain.syntax.rholang.Absyn.VPtStr) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_vptstr.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListValPattern foo, int _i_)
  {
     for (java.util.Iterator<ValPattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListVar foo, int _i_)
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

  private static void pp(coop.rchain.syntax.rholang.Absyn.ListName foo, int _i_)
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


  private static void sh(coop.rchain.syntax.rholang.Absyn.Contr foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.DContr)
    {
       coop.rchain.syntax.rholang.Absyn.DContr _dcontr = (coop.rchain.syntax.rholang.Absyn.DContr) foo;
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

  private static void sh(coop.rchain.syntax.rholang.Absyn.Proc foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PNil)
    {
       coop.rchain.syntax.rholang.Absyn.PNil _pnil = (coop.rchain.syntax.rholang.Absyn.PNil) foo;
       render("PNil");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PValue)
    {
       coop.rchain.syntax.rholang.Absyn.PValue _pvalue = (coop.rchain.syntax.rholang.Absyn.PValue) foo;
       render("(");
       render("PValue");
       sh(_pvalue.value_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PDrop)
    {
       coop.rchain.syntax.rholang.Absyn.PDrop _pdrop = (coop.rchain.syntax.rholang.Absyn.PDrop) foo;
       render("(");
       render("PDrop");
       sh(_pdrop.chan_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PInject)
    {
       coop.rchain.syntax.rholang.Absyn.PInject _pinject = (coop.rchain.syntax.rholang.Absyn.PInject) foo;
       render("(");
       render("PInject");
       sh(_pinject.chan_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PLift)
    {
       coop.rchain.syntax.rholang.Absyn.PLift _plift = (coop.rchain.syntax.rholang.Absyn.PLift) foo;
       render("(");
       render("PLift");
       sh(_plift.chan_);
       render("[");
       sh(_plift.listproc_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PFoldL)
    {
       coop.rchain.syntax.rholang.Absyn.PFoldL _pfoldl = (coop.rchain.syntax.rholang.Absyn.PFoldL) foo;
       render("(");
       render("PFoldL");
       sh(_pfoldl.bind_1);
       sh(_pfoldl.bind_2);
       sh(_pfoldl.proc_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PFoldR)
    {
       coop.rchain.syntax.rholang.Absyn.PFoldR _pfoldr = (coop.rchain.syntax.rholang.Absyn.PFoldR) foo;
       render("(");
       render("PFoldR");
       sh(_pfoldr.bind_1);
       sh(_pfoldr.bind_2);
       sh(_pfoldr.proc_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PInput)
    {
       coop.rchain.syntax.rholang.Absyn.PInput _pinput = (coop.rchain.syntax.rholang.Absyn.PInput) foo;
       render("(");
       render("PInput");
       render("[");
       sh(_pinput.listbind_);
       render("]");
       sh(_pinput.proc_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PChoice)
    {
       coop.rchain.syntax.rholang.Absyn.PChoice _pchoice = (coop.rchain.syntax.rholang.Absyn.PChoice) foo;
       render("(");
       render("PChoice");
       render("[");
       sh(_pchoice.listcbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PMatch _pmatch = (coop.rchain.syntax.rholang.Absyn.PMatch) foo;
       render("(");
       render("PMatch");
       sh(_pmatch.proc_);
       render("[");
       sh(_pmatch.listpmbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PNew)
    {
       coop.rchain.syntax.rholang.Absyn.PNew _pnew = (coop.rchain.syntax.rholang.Absyn.PNew) foo;
       render("(");
       render("PNew");
       render("[");
       sh(_pnew.listvar_);
       render("]");
       sh(_pnew.proc_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PConstr)
    {
       coop.rchain.syntax.rholang.Absyn.PConstr _pconstr = (coop.rchain.syntax.rholang.Absyn.PConstr) foo;
       render("(");
       render("PConstr");
       sh(_pconstr.name_);
       render("[");
       sh(_pconstr.listproc_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPar)
    {
       coop.rchain.syntax.rholang.Absyn.PPar _ppar = (coop.rchain.syntax.rholang.Absyn.PPar) foo;
       render("(");
       render("PPar");
       sh(_ppar.proc_1);
       sh(_ppar.proc_2);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListProc foo)
  {
     for (java.util.Iterator<Proc> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.Chan foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CVar)
    {
       coop.rchain.syntax.rholang.Absyn.CVar _cvar = (coop.rchain.syntax.rholang.Absyn.CVar) foo;
       render("(");
       render("CVar");
       sh(_cvar.var_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CQuote)
    {
       coop.rchain.syntax.rholang.Absyn.CQuote _cquote = (coop.rchain.syntax.rholang.Absyn.CQuote) foo;
       render("(");
       render("CQuote");
       sh(_cquote.proc_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.Bind foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.InputBind)
    {
       coop.rchain.syntax.rholang.Absyn.InputBind _inputbind = (coop.rchain.syntax.rholang.Absyn.InputBind) foo;
       render("(");
       render("InputBind");
       sh(_inputbind.cpattern_);
       sh(_inputbind.chan_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CondInputBind)
    {
       coop.rchain.syntax.rholang.Absyn.CondInputBind _condinputbind = (coop.rchain.syntax.rholang.Absyn.CondInputBind) foo;
       render("(");
       render("CondInputBind");
       sh(_condinputbind.cpattern_);
       sh(_condinputbind.chan_);
       sh(_condinputbind.proc_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListBind foo)
  {
     for (java.util.Iterator<Bind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.PMBranch foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PatternMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PatternMatch _patternmatch = (coop.rchain.syntax.rholang.Absyn.PatternMatch) foo;
       render("(");
       render("PatternMatch");
       sh(_patternmatch.ppattern_);
       sh(_patternmatch.proc_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListPMBranch foo)
  {
     for (java.util.Iterator<PMBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.CBranch foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.Choice)
    {
       coop.rchain.syntax.rholang.Absyn.Choice _choice = (coop.rchain.syntax.rholang.Absyn.Choice) foo;
       render("(");
       render("Choice");
       render("[");
       sh(_choice.listbind_);
       render("]");
       sh(_choice.proc_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListCBranch foo)
  {
     for (java.util.Iterator<CBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.RhoBool foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QTrue)
    {
       coop.rchain.syntax.rholang.Absyn.QTrue _qtrue = (coop.rchain.syntax.rholang.Absyn.QTrue) foo;
       render("QTrue");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QFalse)
    {
       coop.rchain.syntax.rholang.Absyn.QFalse _qfalse = (coop.rchain.syntax.rholang.Absyn.QFalse) foo;
       render("QFalse");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.Quantity foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QBool)
    {
       coop.rchain.syntax.rholang.Absyn.QBool _qbool = (coop.rchain.syntax.rholang.Absyn.QBool) foo;
       render("(");
       render("QBool");
       sh(_qbool.rhobool_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QInt)
    {
       coop.rchain.syntax.rholang.Absyn.QInt _qint = (coop.rchain.syntax.rholang.Absyn.QInt) foo;
       render("(");
       render("QInt");
       sh(_qint.integer_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDouble)
    {
       coop.rchain.syntax.rholang.Absyn.QDouble _qdouble = (coop.rchain.syntax.rholang.Absyn.QDouble) foo;
       render("(");
       render("QDouble");
       sh(_qdouble.double_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QString)
    {
       coop.rchain.syntax.rholang.Absyn.QString _qstring = (coop.rchain.syntax.rholang.Absyn.QString) foo;
       render("(");
       render("QString");
       sh(_qstring.string_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QVar)
    {
       coop.rchain.syntax.rholang.Absyn.QVar _qvar = (coop.rchain.syntax.rholang.Absyn.QVar) foo;
       render("(");
       render("QVar");
       sh(_qvar.var_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDot)
    {
       coop.rchain.syntax.rholang.Absyn.QDot _qdot = (coop.rchain.syntax.rholang.Absyn.QDot) foo;
       render("(");
       render("QDot");
       sh(_qdot.quantity_);
       sh(_qdot.var_);
       render("[");
       sh(_qdot.listquantity_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QNeg)
    {
       coop.rchain.syntax.rholang.Absyn.QNeg _qneg = (coop.rchain.syntax.rholang.Absyn.QNeg) foo;
       render("(");
       render("QNeg");
       sh(_qneg.quantity_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QMult)
    {
       coop.rchain.syntax.rholang.Absyn.QMult _qmult = (coop.rchain.syntax.rholang.Absyn.QMult) foo;
       render("(");
       render("QMult");
       sh(_qmult.quantity_1);
       sh(_qmult.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QDiv)
    {
       coop.rchain.syntax.rholang.Absyn.QDiv _qdiv = (coop.rchain.syntax.rholang.Absyn.QDiv) foo;
       render("(");
       render("QDiv");
       sh(_qdiv.quantity_1);
       sh(_qdiv.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QAdd)
    {
       coop.rchain.syntax.rholang.Absyn.QAdd _qadd = (coop.rchain.syntax.rholang.Absyn.QAdd) foo;
       render("(");
       render("QAdd");
       sh(_qadd.quantity_1);
       sh(_qadd.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QMinus)
    {
       coop.rchain.syntax.rholang.Absyn.QMinus _qminus = (coop.rchain.syntax.rholang.Absyn.QMinus) foo;
       render("(");
       render("QMinus");
       sh(_qminus.quantity_1);
       sh(_qminus.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QLt)
    {
       coop.rchain.syntax.rholang.Absyn.QLt _qlt = (coop.rchain.syntax.rholang.Absyn.QLt) foo;
       render("(");
       render("QLt");
       sh(_qlt.quantity_1);
       sh(_qlt.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QLte)
    {
       coop.rchain.syntax.rholang.Absyn.QLte _qlte = (coop.rchain.syntax.rholang.Absyn.QLte) foo;
       render("(");
       render("QLte");
       sh(_qlte.quantity_1);
       sh(_qlte.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QGt)
    {
       coop.rchain.syntax.rholang.Absyn.QGt _qgt = (coop.rchain.syntax.rholang.Absyn.QGt) foo;
       render("(");
       render("QGt");
       sh(_qgt.quantity_1);
       sh(_qgt.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QGte)
    {
       coop.rchain.syntax.rholang.Absyn.QGte _qgte = (coop.rchain.syntax.rholang.Absyn.QGte) foo;
       render("(");
       render("QGte");
       sh(_qgte.quantity_1);
       sh(_qgte.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QEq)
    {
       coop.rchain.syntax.rholang.Absyn.QEq _qeq = (coop.rchain.syntax.rholang.Absyn.QEq) foo;
       render("(");
       render("QEq");
       sh(_qeq.quantity_1);
       sh(_qeq.quantity_2);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.QNeq)
    {
       coop.rchain.syntax.rholang.Absyn.QNeq _qneq = (coop.rchain.syntax.rholang.Absyn.QNeq) foo;
       render("(");
       render("QNeq");
       sh(_qneq.quantity_1);
       sh(_qneq.quantity_2);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListQuantity foo)
  {
     for (java.util.Iterator<Quantity> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.Value foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VQuant)
    {
       coop.rchain.syntax.rholang.Absyn.VQuant _vquant = (coop.rchain.syntax.rholang.Absyn.VQuant) foo;
       render("(");
       render("VQuant");
       sh(_vquant.quantity_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.EChar)
    {
       coop.rchain.syntax.rholang.Absyn.EChar _echar = (coop.rchain.syntax.rholang.Absyn.EChar) foo;
       render("(");
       render("EChar");
       sh(_echar.char_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.ETuple)
    {
       coop.rchain.syntax.rholang.Absyn.ETuple _etuple = (coop.rchain.syntax.rholang.Absyn.ETuple) foo;
       render("(");
       render("ETuple");
       render("[");
       sh(_etuple.listproc_);
       render("]");
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.VarPattern foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VarPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.VarPtVar _varptvar = (coop.rchain.syntax.rholang.Absyn.VarPtVar) foo;
       render("(");
       render("VarPtVar");
       sh(_varptvar.var_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VarPtWild)
    {
       coop.rchain.syntax.rholang.Absyn.VarPtWild _varptwild = (coop.rchain.syntax.rholang.Absyn.VarPtWild) foo;
       render("VarPtWild");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListVarPattern foo)
  {
     for (java.util.Iterator<VarPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.PPattern foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.PPtVar _pptvar = (coop.rchain.syntax.rholang.Absyn.PPtVar) foo;
       render("(");
       render("PPtVar");
       sh(_pptvar.varpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtNil)
    {
       coop.rchain.syntax.rholang.Absyn.PPtNil _pptnil = (coop.rchain.syntax.rholang.Absyn.PPtNil) foo;
       render("PPtNil");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtVal)
    {
       coop.rchain.syntax.rholang.Absyn.PPtVal _pptval = (coop.rchain.syntax.rholang.Absyn.PPtVal) foo;
       render("(");
       render("PPtVal");
       sh(_pptval.valpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtDrop)
    {
       coop.rchain.syntax.rholang.Absyn.PPtDrop _pptdrop = (coop.rchain.syntax.rholang.Absyn.PPtDrop) foo;
       render("(");
       render("PPtDrop");
       sh(_pptdrop.cpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtInject)
    {
       coop.rchain.syntax.rholang.Absyn.PPtInject _pptinject = (coop.rchain.syntax.rholang.Absyn.PPtInject) foo;
       render("(");
       render("PPtInject");
       sh(_pptinject.cpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtOutput)
    {
       coop.rchain.syntax.rholang.Absyn.PPtOutput _pptoutput = (coop.rchain.syntax.rholang.Absyn.PPtOutput) foo;
       render("(");
       render("PPtOutput");
       sh(_pptoutput.cpattern_);
       render("[");
       sh(_pptoutput.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtInput)
    {
       coop.rchain.syntax.rholang.Absyn.PPtInput _pptinput = (coop.rchain.syntax.rholang.Absyn.PPtInput) foo;
       render("(");
       render("PPtInput");
       render("[");
       sh(_pptinput.listpatternbind_);
       render("]");
       sh(_pptinput.ppattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtMatch)
    {
       coop.rchain.syntax.rholang.Absyn.PPtMatch _pptmatch = (coop.rchain.syntax.rholang.Absyn.PPtMatch) foo;
       render("(");
       render("PPtMatch");
       sh(_pptmatch.ppattern_);
       render("[");
       sh(_pptmatch.listpatternpatternmatch_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtNew)
    {
       coop.rchain.syntax.rholang.Absyn.PPtNew _pptnew = (coop.rchain.syntax.rholang.Absyn.PPtNew) foo;
       render("(");
       render("PPtNew");
       render("[");
       sh(_pptnew.listvarpattern_);
       render("]");
       sh(_pptnew.ppattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtConstr)
    {
       coop.rchain.syntax.rholang.Absyn.PPtConstr _pptconstr = (coop.rchain.syntax.rholang.Absyn.PPtConstr) foo;
       render("(");
       render("PPtConstr");
       sh(_pptconstr.name_);
       render("[");
       sh(_pptconstr.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PPtPar)
    {
       coop.rchain.syntax.rholang.Absyn.PPtPar _pptpar = (coop.rchain.syntax.rholang.Absyn.PPtPar) foo;
       render("(");
       render("PPtPar");
       sh(_pptpar.ppattern_1);
       sh(_pptpar.ppattern_2);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListPPattern foo)
  {
     for (java.util.Iterator<PPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.CPattern foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CPtVar)
    {
       coop.rchain.syntax.rholang.Absyn.CPtVar _cptvar = (coop.rchain.syntax.rholang.Absyn.CPtVar) foo;
       render("(");
       render("CPtVar");
       sh(_cptvar.varpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CValPtrn)
    {
       coop.rchain.syntax.rholang.Absyn.CValPtrn _cvalptrn = (coop.rchain.syntax.rholang.Absyn.CValPtrn) foo;
       render("(");
       render("CValPtrn");
       sh(_cvalptrn.valpattern_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.CPtQuote)
    {
       coop.rchain.syntax.rholang.Absyn.CPtQuote _cptquote = (coop.rchain.syntax.rholang.Absyn.CPtQuote) foo;
       render("(");
       render("CPtQuote");
       sh(_cptquote.ppattern_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListCPattern foo)
  {
     for (java.util.Iterator<CPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.PatternBind foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PtBind)
    {
       coop.rchain.syntax.rholang.Absyn.PtBind _ptbind = (coop.rchain.syntax.rholang.Absyn.PtBind) foo;
       render("(");
       render("PtBind");
       sh(_ptbind.cpattern_1);
       sh(_ptbind.cpattern_2);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListPatternBind foo)
  {
     for (java.util.Iterator<PatternBind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.PatternPatternMatch foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.PtBranch)
    {
       coop.rchain.syntax.rholang.Absyn.PtBranch _ptbranch = (coop.rchain.syntax.rholang.Absyn.PtBranch) foo;
       render("(");
       render("PtBranch");
       sh(_ptbranch.ppattern_1);
       sh(_ptbranch.ppattern_2);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListPatternPatternMatch foo)
  {
     for (java.util.Iterator<PatternPatternMatch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ValPattern foo)
  {
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtStruct)
    {
       coop.rchain.syntax.rholang.Absyn.VPtStruct _vptstruct = (coop.rchain.syntax.rholang.Absyn.VPtStruct) foo;
       render("(");
       render("VPtStruct");
       sh(_vptstruct.var_);
       render("[");
       sh(_vptstruct.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtTuple)
    {
       coop.rchain.syntax.rholang.Absyn.VPtTuple _vpttuple = (coop.rchain.syntax.rholang.Absyn.VPtTuple) foo;
       render("(");
       render("VPtTuple");
       render("[");
       sh(_vpttuple.listppattern_);
       render("]");
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtTrue)
    {
       coop.rchain.syntax.rholang.Absyn.VPtTrue _vpttrue = (coop.rchain.syntax.rholang.Absyn.VPtTrue) foo;
       render("VPtTrue");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtFalse)
    {
       coop.rchain.syntax.rholang.Absyn.VPtFalse _vptfalse = (coop.rchain.syntax.rholang.Absyn.VPtFalse) foo;
       render("VPtFalse");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtInt)
    {
       coop.rchain.syntax.rholang.Absyn.VPtInt _vptint = (coop.rchain.syntax.rholang.Absyn.VPtInt) foo;
       render("(");
       render("VPtInt");
       sh(_vptint.integer_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtDbl)
    {
       coop.rchain.syntax.rholang.Absyn.VPtDbl _vptdbl = (coop.rchain.syntax.rholang.Absyn.VPtDbl) foo;
       render("(");
       render("VPtDbl");
       sh(_vptdbl.double_);
       render(")");
    }
    if (foo instanceof coop.rchain.syntax.rholang.Absyn.VPtStr)
    {
       coop.rchain.syntax.rholang.Absyn.VPtStr _vptstr = (coop.rchain.syntax.rholang.Absyn.VPtStr) foo;
       render("(");
       render("VPtStr");
       sh(_vptstr.string_);
       render(")");
    }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListValPattern foo)
  {
     for (java.util.Iterator<ValPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListVar foo)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(coop.rchain.syntax.rholang.Absyn.ListName foo)
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

