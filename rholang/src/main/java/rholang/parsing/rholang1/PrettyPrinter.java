package rholang.parsing.rholang1;
import rholang.parsing.rholang1.Absyn.*;

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
  public static String print(rholang.parsing.rholang1.Absyn.Contr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Contr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Expr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Expr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Bind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Bind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.PMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.PMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListPMBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListPMBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.CBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.CBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListCBranch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListCBranch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Quantity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Quantity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Entity foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Entity foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Struct foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Struct foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Collect foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Collect foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.VarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.VarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListVarPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListVarPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.Pattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.Pattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.PatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.PatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListPatternBind foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListPatternBind foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.PatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.PatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListPatternPatternMatch foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListPatternPatternMatch foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ValPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ValPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListVar foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListVar foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.rholang1.Absyn.ListName foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.rholang1.Absyn.ListName foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(rholang.parsing.rholang1.Absyn.Contr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.DContr)
    {
       rholang.parsing.rholang1.Absyn.DContr _dcontr = (rholang.parsing.rholang1.Absyn.DContr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("contract");
       pp(_dcontr.name_, 0);
       render("(");
       pp(_dcontr.listpattern_, 0);
       render(")");
       render("=");
       render("{");
       pp(_dcontr.expr_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.Expr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.ENil)
    {
       rholang.parsing.rholang1.Absyn.ENil _enil = (rholang.parsing.rholang1.Absyn.ENil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EValue)
    {
       rholang.parsing.rholang1.Absyn.EValue _evalue = (rholang.parsing.rholang1.Absyn.EValue) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_evalue.value_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EVar)
    {
       rholang.parsing.rholang1.Absyn.EVar _evar = (rholang.parsing.rholang1.Absyn.EVar) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_evar.var_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EDrop)
    {
       rholang.parsing.rholang1.Absyn.EDrop _edrop = (rholang.parsing.rholang1.Absyn.EDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_edrop.expr_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EQuote)
    {
       rholang.parsing.rholang1.Absyn.EQuote _equote = (rholang.parsing.rholang1.Absyn.EQuote) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("@");
       pp(_equote.expr_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EInject)
    {
       rholang.parsing.rholang1.Absyn.EInject _einject = (rholang.parsing.rholang1.Absyn.EInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_einject.expr_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.ELift)
    {
       rholang.parsing.rholang1.Absyn.ELift _elift = (rholang.parsing.rholang1.Absyn.ELift) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_elift.expr_, 3);
       render("!");
       render("(");
       pp(_elift.listexpr_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EInput)
    {
       rholang.parsing.rholang1.Absyn.EInput _einput = (rholang.parsing.rholang1.Absyn.EInput) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("for");
       render("(");
       pp(_einput.listbind_, 0);
       render(")");
       render("{");
       pp(_einput.expr_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EChoice)
    {
       rholang.parsing.rholang1.Absyn.EChoice _echoice = (rholang.parsing.rholang1.Absyn.EChoice) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("select");
       render("{");
       pp(_echoice.listcbranch_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EMatch)
    {
       rholang.parsing.rholang1.Absyn.EMatch _ematch = (rholang.parsing.rholang1.Absyn.EMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_ematch.expr_, 0);
       render("with");
       pp(_ematch.listpmbranch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.ENew)
    {
       rholang.parsing.rholang1.Absyn.ENew _enew = (rholang.parsing.rholang1.Absyn.ENew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_enew.listvar_, 0);
       render("in");
       pp(_enew.expr_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EConstr)
    {
       rholang.parsing.rholang1.Absyn.EConstr _econstr = (rholang.parsing.rholang1.Absyn.EConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_econstr.name_, 0);
       render("(");
       pp(_econstr.listexpr_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EPar)
    {
       rholang.parsing.rholang1.Absyn.EPar _epar = (rholang.parsing.rholang1.Absyn.EPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_epar.expr_1, 0);
       render("|");
       pp(_epar.expr_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListExpr foo, int _i_)
  {
     for (java.util.Iterator<Expr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang1.Absyn.Bind foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.InputBind)
    {
       rholang.parsing.rholang1.Absyn.InputBind _inputbind = (rholang.parsing.rholang1.Absyn.InputBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_inputbind.pattern_, 0);
       render("<-");
       pp(_inputbind.expr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListBind foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.PMBranch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PatternMatch)
    {
       rholang.parsing.rholang1.Absyn.PatternMatch _patternmatch = (rholang.parsing.rholang1.Absyn.PatternMatch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_patternmatch.pattern_, 0);
       render("=>");
       render("{");
       pp(_patternmatch.expr_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListPMBranch foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.CBranch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.Choice)
    {
       rholang.parsing.rholang1.Absyn.Choice _choice = (rholang.parsing.rholang1.Absyn.Choice) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("case");
       pp(_choice.listbind_, 0);
       render("=>");
       render("{");
       pp(_choice.expr_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListCBranch foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.Value foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VQuant)
    {
       rholang.parsing.rholang1.Absyn.VQuant _vquant = (rholang.parsing.rholang1.Absyn.VQuant) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vquant.quantity_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.VEnt)
    {
       rholang.parsing.rholang1.Absyn.VEnt _vent = (rholang.parsing.rholang1.Absyn.VEnt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vent.entity_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.Quantity foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.QInt)
    {
       rholang.parsing.rholang1.Absyn.QInt _qint = (rholang.parsing.rholang1.Absyn.QInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.QDouble)
    {
       rholang.parsing.rholang1.Absyn.QDouble _qdouble = (rholang.parsing.rholang1.Absyn.QDouble) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qdouble.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.Entity foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.EChar)
    {
       rholang.parsing.rholang1.Absyn.EChar _echar = (rholang.parsing.rholang1.Absyn.EChar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_echar.char_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.EStruct)
    {
       rholang.parsing.rholang1.Absyn.EStruct _estruct = (rholang.parsing.rholang1.Absyn.EStruct) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_estruct.struct_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.ECollect)
    {
       rholang.parsing.rholang1.Absyn.ECollect _ecollect = (rholang.parsing.rholang1.Absyn.ECollect) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ecollect.collect_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.Struct foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.StructConstr)
    {
       rholang.parsing.rholang1.Absyn.StructConstr _structconstr = (rholang.parsing.rholang1.Absyn.StructConstr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_structconstr.var_, 0);
       render("{");
       pp(_structconstr.listexpr_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.Collect foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.CString)
    {
       rholang.parsing.rholang1.Absyn.CString _cstring = (rholang.parsing.rholang1.Absyn.CString) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cstring.string_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.VarPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VarPtVar)
    {
       rholang.parsing.rholang1.Absyn.VarPtVar _varptvar = (rholang.parsing.rholang1.Absyn.VarPtVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_varptvar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.VarPtWild)
    {
       rholang.parsing.rholang1.Absyn.VarPtWild _varptwild = (rholang.parsing.rholang1.Absyn.VarPtWild) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListVarPattern foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.Pattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtNil)
    {
       rholang.parsing.rholang1.Absyn.PtNil _ptnil = (rholang.parsing.rholang1.Absyn.PtNil) foo;
       if (_i_ > 4) render(_L_PAREN);
       render("Nil");
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtVar)
    {
       rholang.parsing.rholang1.Absyn.PtVar _ptvar = (rholang.parsing.rholang1.Absyn.PtVar) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_ptvar.varpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtVal)
    {
       rholang.parsing.rholang1.Absyn.PtVal _ptval = (rholang.parsing.rholang1.Absyn.PtVal) foo;
       if (_i_ > 4) render(_L_PAREN);
       pp(_ptval.valpattern_, 0);
       if (_i_ > 4) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtDrop)
    {
       rholang.parsing.rholang1.Absyn.PtDrop _ptdrop = (rholang.parsing.rholang1.Absyn.PtDrop) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("*");
       pp(_ptdrop.pattern_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtInject)
    {
       rholang.parsing.rholang1.Absyn.PtInject _ptinject = (rholang.parsing.rholang1.Absyn.PtInject) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("#");
       pp(_ptinject.pattern_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtQuote)
    {
       rholang.parsing.rholang1.Absyn.PtQuote _ptquote = (rholang.parsing.rholang1.Absyn.PtQuote) foo;
       if (_i_ > 3) render(_L_PAREN);
       render("@");
       pp(_ptquote.pattern_, 3);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtOutput)
    {
       rholang.parsing.rholang1.Absyn.PtOutput _ptoutput = (rholang.parsing.rholang1.Absyn.PtOutput) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_ptoutput.pattern_, 3);
       render("!");
       render("(");
       pp(_ptoutput.listpattern_, 0);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtInput)
    {
       rholang.parsing.rholang1.Absyn.PtInput _ptinput = (rholang.parsing.rholang1.Absyn.PtInput) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("for");
       render("(");
       pp(_ptinput.listpatternbind_, 0);
       render(")");
       render("{");
       pp(_ptinput.pattern_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtMatch)
    {
       rholang.parsing.rholang1.Absyn.PtMatch _ptmatch = (rholang.parsing.rholang1.Absyn.PtMatch) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("match");
       pp(_ptmatch.pattern_, 0);
       render("with");
       pp(_ptmatch.listpatternpatternmatch_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtNew)
    {
       rholang.parsing.rholang1.Absyn.PtNew _ptnew = (rholang.parsing.rholang1.Absyn.PtNew) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("new");
       pp(_ptnew.listvarpattern_, 0);
       render("in");
       pp(_ptnew.pattern_, 1);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtConstr)
    {
       rholang.parsing.rholang1.Absyn.PtConstr _ptconstr = (rholang.parsing.rholang1.Absyn.PtConstr) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_ptconstr.name_, 0);
       render("(");
       pp(_ptconstr.listpattern_, 0);
       render(")");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.rholang1.Absyn.PtPar)
    {
       rholang.parsing.rholang1.Absyn.PtPar _ptpar = (rholang.parsing.rholang1.Absyn.PtPar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptpar.pattern_1, 0);
       render("|");
       pp(_ptpar.pattern_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListPattern foo, int _i_)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }  }

  private static void pp(rholang.parsing.rholang1.Absyn.PatternBind foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtBind)
    {
       rholang.parsing.rholang1.Absyn.PtBind _ptbind = (rholang.parsing.rholang1.Absyn.PtBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbind.pattern_1, 0);
       render("<-");
       pp(_ptbind.pattern_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListPatternBind foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.PatternPatternMatch foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtBranch)
    {
       rholang.parsing.rholang1.Absyn.PtBranch _ptbranch = (rholang.parsing.rholang1.Absyn.PtBranch) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ptbranch.pattern_1, 0);
       render("=>");
       render("{");
       pp(_ptbranch.pattern_2, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListPatternPatternMatch foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.ValPattern foo, int _i_)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VPtStruct)
    {
       rholang.parsing.rholang1.Absyn.VPtStruct _vptstruct = (rholang.parsing.rholang1.Absyn.VPtStruct) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vptstruct.var_, 0);
       render("{");
       pp(_vptstruct.listpattern_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.rholang1.Absyn.ListVar foo, int _i_)
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

  private static void pp(rholang.parsing.rholang1.Absyn.ListName foo, int _i_)
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


  private static void sh(rholang.parsing.rholang1.Absyn.Contr foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.DContr)
    {
       rholang.parsing.rholang1.Absyn.DContr _dcontr = (rholang.parsing.rholang1.Absyn.DContr) foo;
       render("(");
       render("DContr");
       sh(_dcontr.name_);
       render("[");
       sh(_dcontr.listpattern_);
       render("]");
       sh(_dcontr.expr_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Expr foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.ENil)
    {
       rholang.parsing.rholang1.Absyn.ENil _enil = (rholang.parsing.rholang1.Absyn.ENil) foo;
       render("ENil");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EValue)
    {
       rholang.parsing.rholang1.Absyn.EValue _evalue = (rholang.parsing.rholang1.Absyn.EValue) foo;
       render("(");
       render("EValue");
       sh(_evalue.value_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EVar)
    {
       rholang.parsing.rholang1.Absyn.EVar _evar = (rholang.parsing.rholang1.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EDrop)
    {
       rholang.parsing.rholang1.Absyn.EDrop _edrop = (rholang.parsing.rholang1.Absyn.EDrop) foo;
       render("(");
       render("EDrop");
       sh(_edrop.expr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EQuote)
    {
       rholang.parsing.rholang1.Absyn.EQuote _equote = (rholang.parsing.rholang1.Absyn.EQuote) foo;
       render("(");
       render("EQuote");
       sh(_equote.expr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EInject)
    {
       rholang.parsing.rholang1.Absyn.EInject _einject = (rholang.parsing.rholang1.Absyn.EInject) foo;
       render("(");
       render("EInject");
       sh(_einject.expr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.ELift)
    {
       rholang.parsing.rholang1.Absyn.ELift _elift = (rholang.parsing.rholang1.Absyn.ELift) foo;
       render("(");
       render("ELift");
       sh(_elift.expr_);
       render("[");
       sh(_elift.listexpr_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EInput)
    {
       rholang.parsing.rholang1.Absyn.EInput _einput = (rholang.parsing.rholang1.Absyn.EInput) foo;
       render("(");
       render("EInput");
       render("[");
       sh(_einput.listbind_);
       render("]");
       sh(_einput.expr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EChoice)
    {
       rholang.parsing.rholang1.Absyn.EChoice _echoice = (rholang.parsing.rholang1.Absyn.EChoice) foo;
       render("(");
       render("EChoice");
       render("[");
       sh(_echoice.listcbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EMatch)
    {
       rholang.parsing.rholang1.Absyn.EMatch _ematch = (rholang.parsing.rholang1.Absyn.EMatch) foo;
       render("(");
       render("EMatch");
       sh(_ematch.expr_);
       render("[");
       sh(_ematch.listpmbranch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.ENew)
    {
       rholang.parsing.rholang1.Absyn.ENew _enew = (rholang.parsing.rholang1.Absyn.ENew) foo;
       render("(");
       render("ENew");
       render("[");
       sh(_enew.listvar_);
       render("]");
       sh(_enew.expr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EConstr)
    {
       rholang.parsing.rholang1.Absyn.EConstr _econstr = (rholang.parsing.rholang1.Absyn.EConstr) foo;
       render("(");
       render("EConstr");
       sh(_econstr.name_);
       render("[");
       sh(_econstr.listexpr_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EPar)
    {
       rholang.parsing.rholang1.Absyn.EPar _epar = (rholang.parsing.rholang1.Absyn.EPar) foo;
       render("(");
       render("EPar");
       sh(_epar.expr_1);
       sh(_epar.expr_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListExpr foo)
  {
     for (java.util.Iterator<Expr> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Bind foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.InputBind)
    {
       rholang.parsing.rholang1.Absyn.InputBind _inputbind = (rholang.parsing.rholang1.Absyn.InputBind) foo;
       render("(");
       render("InputBind");
       sh(_inputbind.pattern_);
       sh(_inputbind.expr_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListBind foo)
  {
     for (java.util.Iterator<Bind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.PMBranch foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PatternMatch)
    {
       rholang.parsing.rholang1.Absyn.PatternMatch _patternmatch = (rholang.parsing.rholang1.Absyn.PatternMatch) foo;
       render("(");
       render("PatternMatch");
       sh(_patternmatch.pattern_);
       sh(_patternmatch.expr_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListPMBranch foo)
  {
     for (java.util.Iterator<PMBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.CBranch foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.Choice)
    {
       rholang.parsing.rholang1.Absyn.Choice _choice = (rholang.parsing.rholang1.Absyn.Choice) foo;
       render("(");
       render("Choice");
       render("[");
       sh(_choice.listbind_);
       render("]");
       sh(_choice.expr_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListCBranch foo)
  {
     for (java.util.Iterator<CBranch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Value foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VQuant)
    {
       rholang.parsing.rholang1.Absyn.VQuant _vquant = (rholang.parsing.rholang1.Absyn.VQuant) foo;
       render("(");
       render("VQuant");
       sh(_vquant.quantity_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.VEnt)
    {
       rholang.parsing.rholang1.Absyn.VEnt _vent = (rholang.parsing.rholang1.Absyn.VEnt) foo;
       render("(");
       render("VEnt");
       sh(_vent.entity_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Quantity foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.QInt)
    {
       rholang.parsing.rholang1.Absyn.QInt _qint = (rholang.parsing.rholang1.Absyn.QInt) foo;
       render("(");
       render("QInt");
       sh(_qint.integer_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.QDouble)
    {
       rholang.parsing.rholang1.Absyn.QDouble _qdouble = (rholang.parsing.rholang1.Absyn.QDouble) foo;
       render("(");
       render("QDouble");
       sh(_qdouble.double_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Entity foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.EChar)
    {
       rholang.parsing.rholang1.Absyn.EChar _echar = (rholang.parsing.rholang1.Absyn.EChar) foo;
       render("(");
       render("EChar");
       sh(_echar.char_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.EStruct)
    {
       rholang.parsing.rholang1.Absyn.EStruct _estruct = (rholang.parsing.rholang1.Absyn.EStruct) foo;
       render("(");
       render("EStruct");
       sh(_estruct.struct_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.ECollect)
    {
       rholang.parsing.rholang1.Absyn.ECollect _ecollect = (rholang.parsing.rholang1.Absyn.ECollect) foo;
       render("(");
       render("ECollect");
       sh(_ecollect.collect_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Struct foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.StructConstr)
    {
       rholang.parsing.rholang1.Absyn.StructConstr _structconstr = (rholang.parsing.rholang1.Absyn.StructConstr) foo;
       render("(");
       render("StructConstr");
       sh(_structconstr.var_);
       render("[");
       sh(_structconstr.listexpr_);
       render("]");
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Collect foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.CString)
    {
       rholang.parsing.rholang1.Absyn.CString _cstring = (rholang.parsing.rholang1.Absyn.CString) foo;
       render("(");
       render("CString");
       sh(_cstring.string_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.VarPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VarPtVar)
    {
       rholang.parsing.rholang1.Absyn.VarPtVar _varptvar = (rholang.parsing.rholang1.Absyn.VarPtVar) foo;
       render("(");
       render("VarPtVar");
       sh(_varptvar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.VarPtWild)
    {
       rholang.parsing.rholang1.Absyn.VarPtWild _varptwild = (rholang.parsing.rholang1.Absyn.VarPtWild) foo;
       render("VarPtWild");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListVarPattern foo)
  {
     for (java.util.Iterator<VarPattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.Pattern foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtNil)
    {
       rholang.parsing.rholang1.Absyn.PtNil _ptnil = (rholang.parsing.rholang1.Absyn.PtNil) foo;
       render("PtNil");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtVar)
    {
       rholang.parsing.rholang1.Absyn.PtVar _ptvar = (rholang.parsing.rholang1.Absyn.PtVar) foo;
       render("(");
       render("PtVar");
       sh(_ptvar.varpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtVal)
    {
       rholang.parsing.rholang1.Absyn.PtVal _ptval = (rholang.parsing.rholang1.Absyn.PtVal) foo;
       render("(");
       render("PtVal");
       sh(_ptval.valpattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtDrop)
    {
       rholang.parsing.rholang1.Absyn.PtDrop _ptdrop = (rholang.parsing.rholang1.Absyn.PtDrop) foo;
       render("(");
       render("PtDrop");
       sh(_ptdrop.pattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtInject)
    {
       rholang.parsing.rholang1.Absyn.PtInject _ptinject = (rholang.parsing.rholang1.Absyn.PtInject) foo;
       render("(");
       render("PtInject");
       sh(_ptinject.pattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtQuote)
    {
       rholang.parsing.rholang1.Absyn.PtQuote _ptquote = (rholang.parsing.rholang1.Absyn.PtQuote) foo;
       render("(");
       render("PtQuote");
       sh(_ptquote.pattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtOutput)
    {
       rholang.parsing.rholang1.Absyn.PtOutput _ptoutput = (rholang.parsing.rholang1.Absyn.PtOutput) foo;
       render("(");
       render("PtOutput");
       sh(_ptoutput.pattern_);
       render("[");
       sh(_ptoutput.listpattern_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtInput)
    {
       rholang.parsing.rholang1.Absyn.PtInput _ptinput = (rholang.parsing.rholang1.Absyn.PtInput) foo;
       render("(");
       render("PtInput");
       render("[");
       sh(_ptinput.listpatternbind_);
       render("]");
       sh(_ptinput.pattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtMatch)
    {
       rholang.parsing.rholang1.Absyn.PtMatch _ptmatch = (rholang.parsing.rholang1.Absyn.PtMatch) foo;
       render("(");
       render("PtMatch");
       sh(_ptmatch.pattern_);
       render("[");
       sh(_ptmatch.listpatternpatternmatch_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtNew)
    {
       rholang.parsing.rholang1.Absyn.PtNew _ptnew = (rholang.parsing.rholang1.Absyn.PtNew) foo;
       render("(");
       render("PtNew");
       render("[");
       sh(_ptnew.listvarpattern_);
       render("]");
       sh(_ptnew.pattern_);
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtConstr)
    {
       rholang.parsing.rholang1.Absyn.PtConstr _ptconstr = (rholang.parsing.rholang1.Absyn.PtConstr) foo;
       render("(");
       render("PtConstr");
       sh(_ptconstr.name_);
       render("[");
       sh(_ptconstr.listpattern_);
       render("]");
       render(")");
    }
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtPar)
    {
       rholang.parsing.rholang1.Absyn.PtPar _ptpar = (rholang.parsing.rholang1.Absyn.PtPar) foo;
       render("(");
       render("PtPar");
       sh(_ptpar.pattern_1);
       sh(_ptpar.pattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListPattern foo)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.PatternBind foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtBind)
    {
       rholang.parsing.rholang1.Absyn.PtBind _ptbind = (rholang.parsing.rholang1.Absyn.PtBind) foo;
       render("(");
       render("PtBind");
       sh(_ptbind.pattern_1);
       sh(_ptbind.pattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListPatternBind foo)
  {
     for (java.util.Iterator<PatternBind> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.PatternPatternMatch foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.PtBranch)
    {
       rholang.parsing.rholang1.Absyn.PtBranch _ptbranch = (rholang.parsing.rholang1.Absyn.PtBranch) foo;
       render("(");
       render("PtBranch");
       sh(_ptbranch.pattern_1);
       sh(_ptbranch.pattern_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListPatternPatternMatch foo)
  {
     for (java.util.Iterator<PatternPatternMatch> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ValPattern foo)
  {
    if (foo instanceof rholang.parsing.rholang1.Absyn.VPtStruct)
    {
       rholang.parsing.rholang1.Absyn.VPtStruct _vptstruct = (rholang.parsing.rholang1.Absyn.VPtStruct) foo;
       render("(");
       render("VPtStruct");
       sh(_vptstruct.var_);
       render("[");
       sh(_vptstruct.listpattern_);
       render("]");
       render(")");
    }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListVar foo)
  {
     for (java.util.Iterator<String> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(rholang.parsing.rholang1.Absyn.ListName foo)
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

