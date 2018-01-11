package rholang.parsing.delimc;
import rholang.parsing.delimc.Absyn.*;

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
  public static String print(rholang.parsing.delimc.Absyn.TypedExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.TypedExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.delimc.Absyn.Expr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.Expr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.delimc.Absyn.Tuple foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.Tuple foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.delimc.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.delimc.Absyn.Type foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.Type foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.delimc.Absyn.TType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.delimc.Absyn.TType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(rholang.parsing.delimc.Absyn.TypedExpr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.ETyped)
    {
       rholang.parsing.delimc.Absyn.ETyped _etyped = (rholang.parsing.delimc.Absyn.ETyped) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_etyped.expr_, 0);
       render(":");
       pp(_etyped.type_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.delimc.Absyn.Expr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.EVar)
    {
       rholang.parsing.delimc.Absyn.EVar _evar = (rholang.parsing.delimc.Absyn.EVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_evar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EVal)
    {
       rholang.parsing.delimc.Absyn.EVal _eval = (rholang.parsing.delimc.Absyn.EVal) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eval.value_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EAbs)
    {
       rholang.parsing.delimc.Absyn.EAbs _eabs = (rholang.parsing.delimc.Absyn.EAbs) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\\");
       pp(_eabs.var_, 0);
       render(":");
       pp(_eabs.type_, 0);
       render(".");
       pp(_eabs.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EApp)
    {
       rholang.parsing.delimc.Absyn.EApp _eapp = (rholang.parsing.delimc.Absyn.EApp) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eapp.typedexpr_1, 0);
       pp(_eapp.typedexpr_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EReturn)
    {
       rholang.parsing.delimc.Absyn.EReturn _ereturn = (rholang.parsing.delimc.Absyn.EReturn) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("return");
       pp(_ereturn.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EBind)
    {
       rholang.parsing.delimc.Absyn.EBind _ebind = (rholang.parsing.delimc.Absyn.EBind) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ebind.typedexpr_1, 0);
       render(">>=");
       pp(_ebind.typedexpr_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.ENewPrompt)
    {
       rholang.parsing.delimc.Absyn.ENewPrompt _enewprompt = (rholang.parsing.delimc.Absyn.ENewPrompt) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("newPrompt");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EPushPrompt)
    {
       rholang.parsing.delimc.Absyn.EPushPrompt _epushprompt = (rholang.parsing.delimc.Absyn.EPushPrompt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_epushprompt.typedexpr_1, 0);
       pp(_epushprompt.typedexpr_2, 0);
       render("pushPrompt");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EWithSubCont)
    {
       rholang.parsing.delimc.Absyn.EWithSubCont _ewithsubcont = (rholang.parsing.delimc.Absyn.EWithSubCont) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ewithsubcont.typedexpr_1, 0);
       pp(_ewithsubcont.typedexpr_2, 0);
       render("withSubCont");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.EPushSubCont)
    {
       rholang.parsing.delimc.Absyn.EPushSubCont _epushsubcont = (rholang.parsing.delimc.Absyn.EPushSubCont) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_epushsubcont.typedexpr_1, 0);
       pp(_epushsubcont.typedexpr_2, 0);
       render("pushSubCont");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.ETuple)
    {
       rholang.parsing.delimc.Absyn.ETuple _etuple = (rholang.parsing.delimc.Absyn.ETuple) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_etuple.tuple_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.delimc.Absyn.Tuple foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.Tuple2)
    {
       rholang.parsing.delimc.Absyn.Tuple2 _tuple2 = (rholang.parsing.delimc.Absyn.Tuple2) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_tuple2.typedexpr_1, 0);
       render(",");
       pp(_tuple2.typedexpr_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.Tuple3)
    {
       rholang.parsing.delimc.Absyn.Tuple3 _tuple3 = (rholang.parsing.delimc.Absyn.Tuple3) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_tuple3.typedexpr_1, 0);
       render(",");
       pp(_tuple3.typedexpr_2, 0);
       render(",");
       pp(_tuple3.typedexpr_3, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.delimc.Absyn.Value foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.VInt)
    {
       rholang.parsing.delimc.Absyn.VInt _vint = (rholang.parsing.delimc.Absyn.VInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.VString)
    {
       rholang.parsing.delimc.Absyn.VString _vstring = (rholang.parsing.delimc.Absyn.VString) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vstring.string_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.delimc.Absyn.Type foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.TSimple)
    {
       rholang.parsing.delimc.Absyn.TSimple _tsimple = (rholang.parsing.delimc.Absyn.TSimple) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_tsimple.simpletype_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.TTuple)
    {
       rholang.parsing.delimc.Absyn.TTuple _ttuple = (rholang.parsing.delimc.Absyn.TTuple) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_ttuple.ttype_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.TMonad)
    {
       rholang.parsing.delimc.Absyn.TMonad _tmonad = (rholang.parsing.delimc.Absyn.TMonad) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("CC");
       pp(_tmonad.type_1, 1);
       pp(_tmonad.type_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.TFun)
    {
       rholang.parsing.delimc.Absyn.TFun _tfun = (rholang.parsing.delimc.Absyn.TFun) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tfun.type_1, 1);
       render("->");
       pp(_tfun.type_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.delimc.Absyn.TType foo, int _i_)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.TType2)
    {
       rholang.parsing.delimc.Absyn.TType2 _ttype2 = (rholang.parsing.delimc.Absyn.TType2) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_ttype2.type_1, 0);
       render(",");
       pp(_ttype2.type_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.delimc.Absyn.TType3)
    {
       rholang.parsing.delimc.Absyn.TType3 _ttype3 = (rholang.parsing.delimc.Absyn.TType3) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_ttype3.type_1, 0);
       render(",");
       pp(_ttype3.type_2, 0);
       render(",");
       pp(_ttype3.type_3, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }


  private static void sh(rholang.parsing.delimc.Absyn.TypedExpr foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.ETyped)
    {
       rholang.parsing.delimc.Absyn.ETyped _etyped = (rholang.parsing.delimc.Absyn.ETyped) foo;
       render("(");
       render("ETyped");
       sh(_etyped.expr_);
       sh(_etyped.type_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.delimc.Absyn.Expr foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.EVar)
    {
       rholang.parsing.delimc.Absyn.EVar _evar = (rholang.parsing.delimc.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EVal)
    {
       rholang.parsing.delimc.Absyn.EVal _eval = (rholang.parsing.delimc.Absyn.EVal) foo;
       render("(");
       render("EVal");
       sh(_eval.value_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EAbs)
    {
       rholang.parsing.delimc.Absyn.EAbs _eabs = (rholang.parsing.delimc.Absyn.EAbs) foo;
       render("(");
       render("EAbs");
       sh(_eabs.var_);
       sh(_eabs.type_);
       sh(_eabs.typedexpr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EApp)
    {
       rholang.parsing.delimc.Absyn.EApp _eapp = (rholang.parsing.delimc.Absyn.EApp) foo;
       render("(");
       render("EApp");
       sh(_eapp.typedexpr_1);
       sh(_eapp.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EReturn)
    {
       rholang.parsing.delimc.Absyn.EReturn _ereturn = (rholang.parsing.delimc.Absyn.EReturn) foo;
       render("(");
       render("EReturn");
       sh(_ereturn.typedexpr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EBind)
    {
       rholang.parsing.delimc.Absyn.EBind _ebind = (rholang.parsing.delimc.Absyn.EBind) foo;
       render("(");
       render("EBind");
       sh(_ebind.typedexpr_1);
       sh(_ebind.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.ENewPrompt)
    {
       rholang.parsing.delimc.Absyn.ENewPrompt _enewprompt = (rholang.parsing.delimc.Absyn.ENewPrompt) foo;
       render("ENewPrompt");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EPushPrompt)
    {
       rholang.parsing.delimc.Absyn.EPushPrompt _epushprompt = (rholang.parsing.delimc.Absyn.EPushPrompt) foo;
       render("(");
       render("EPushPrompt");
       sh(_epushprompt.typedexpr_1);
       sh(_epushprompt.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EWithSubCont)
    {
       rholang.parsing.delimc.Absyn.EWithSubCont _ewithsubcont = (rholang.parsing.delimc.Absyn.EWithSubCont) foo;
       render("(");
       render("EWithSubCont");
       sh(_ewithsubcont.typedexpr_1);
       sh(_ewithsubcont.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.EPushSubCont)
    {
       rholang.parsing.delimc.Absyn.EPushSubCont _epushsubcont = (rholang.parsing.delimc.Absyn.EPushSubCont) foo;
       render("(");
       render("EPushSubCont");
       sh(_epushsubcont.typedexpr_1);
       sh(_epushsubcont.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.ETuple)
    {
       rholang.parsing.delimc.Absyn.ETuple _etuple = (rholang.parsing.delimc.Absyn.ETuple) foo;
       render("(");
       render("ETuple");
       sh(_etuple.tuple_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.delimc.Absyn.Tuple foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.Tuple2)
    {
       rholang.parsing.delimc.Absyn.Tuple2 _tuple2 = (rholang.parsing.delimc.Absyn.Tuple2) foo;
       render("(");
       render("Tuple2");
       sh(_tuple2.typedexpr_1);
       sh(_tuple2.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.Tuple3)
    {
       rholang.parsing.delimc.Absyn.Tuple3 _tuple3 = (rholang.parsing.delimc.Absyn.Tuple3) foo;
       render("(");
       render("Tuple3");
       sh(_tuple3.typedexpr_1);
       sh(_tuple3.typedexpr_2);
       sh(_tuple3.typedexpr_3);
       render(")");
    }
  }

  private static void sh(rholang.parsing.delimc.Absyn.Value foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.VInt)
    {
       rholang.parsing.delimc.Absyn.VInt _vint = (rholang.parsing.delimc.Absyn.VInt) foo;
       render("(");
       render("VInt");
       sh(_vint.integer_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.VString)
    {
       rholang.parsing.delimc.Absyn.VString _vstring = (rholang.parsing.delimc.Absyn.VString) foo;
       render("(");
       render("VString");
       sh(_vstring.string_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.delimc.Absyn.Type foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.TSimple)
    {
       rholang.parsing.delimc.Absyn.TSimple _tsimple = (rholang.parsing.delimc.Absyn.TSimple) foo;
       render("(");
       render("TSimple");
       sh(_tsimple.simpletype_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.TTuple)
    {
       rholang.parsing.delimc.Absyn.TTuple _ttuple = (rholang.parsing.delimc.Absyn.TTuple) foo;
       render("(");
       render("TTuple");
       sh(_ttuple.ttype_);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.TMonad)
    {
       rholang.parsing.delimc.Absyn.TMonad _tmonad = (rholang.parsing.delimc.Absyn.TMonad) foo;
       render("(");
       render("TMonad");
       sh(_tmonad.type_1);
       sh(_tmonad.type_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.TFun)
    {
       rholang.parsing.delimc.Absyn.TFun _tfun = (rholang.parsing.delimc.Absyn.TFun) foo;
       render("(");
       render("TFun");
       sh(_tfun.type_1);
       sh(_tfun.type_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.delimc.Absyn.TType foo)
  {
    if (foo instanceof rholang.parsing.delimc.Absyn.TType2)
    {
       rholang.parsing.delimc.Absyn.TType2 _ttype2 = (rholang.parsing.delimc.Absyn.TType2) foo;
       render("(");
       render("TType2");
       sh(_ttype2.type_1);
       sh(_ttype2.type_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.delimc.Absyn.TType3)
    {
       rholang.parsing.delimc.Absyn.TType3 _ttype3 = (rholang.parsing.delimc.Absyn.TType3) foo;
       render("(");
       render("TType3");
       sh(_ttype3.type_1);
       sh(_ttype3.type_2);
       sh(_ttype3.type_3);
       render(")");
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

