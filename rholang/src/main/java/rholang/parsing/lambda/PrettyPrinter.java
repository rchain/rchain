package rholang.parsing.lambda;
import rholang.parsing.lambda.Absyn.*;

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
  public static String print(rholang.parsing.lambda.Absyn.TypedExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.TypedExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.lambda.Absyn.Expr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.Expr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.lambda.Absyn.Tuple foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.Tuple foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.lambda.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.lambda.Absyn.Type foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.Type foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(rholang.parsing.lambda.Absyn.TType foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(rholang.parsing.lambda.Absyn.TType foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(rholang.parsing.lambda.Absyn.TypedExpr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.ETyped)
    {
       rholang.parsing.lambda.Absyn.ETyped _etyped = (rholang.parsing.lambda.Absyn.ETyped) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_etyped.expr_, 0);
       render(":");
       pp(_etyped.type_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.lambda.Absyn.Expr foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.EVar)
    {
       rholang.parsing.lambda.Absyn.EVar _evar = (rholang.parsing.lambda.Absyn.EVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_evar.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.EVal)
    {
       rholang.parsing.lambda.Absyn.EVal _eval = (rholang.parsing.lambda.Absyn.EVal) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eval.value_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.EAbs)
    {
       rholang.parsing.lambda.Absyn.EAbs _eabs = (rholang.parsing.lambda.Absyn.EAbs) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\\");
       pp(_eabs.var_, 0);
       render(":");
       pp(_eabs.type_, 0);
       render(".");
       pp(_eabs.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.EApp)
    {
       rholang.parsing.lambda.Absyn.EApp _eapp = (rholang.parsing.lambda.Absyn.EApp) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eapp.typedexpr_1, 0);
       pp(_eapp.typedexpr_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.ETuple)
    {
       rholang.parsing.lambda.Absyn.ETuple _etuple = (rholang.parsing.lambda.Absyn.ETuple) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_etuple.tuple_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.EFirst)
    {
       rholang.parsing.lambda.Absyn.EFirst _efirst = (rholang.parsing.lambda.Absyn.EFirst) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("fst");
       pp(_efirst.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.ESecond)
    {
       rholang.parsing.lambda.Absyn.ESecond _esecond = (rholang.parsing.lambda.Absyn.ESecond) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("snd");
       pp(_esecond.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.EThird)
    {
       rholang.parsing.lambda.Absyn.EThird _ethird = (rholang.parsing.lambda.Absyn.EThird) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("thd");
       pp(_ethird.typedexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.lambda.Absyn.Tuple foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.Tuple2)
    {
       rholang.parsing.lambda.Absyn.Tuple2 _tuple2 = (rholang.parsing.lambda.Absyn.Tuple2) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_tuple2.typedexpr_1, 0);
       render(",");
       pp(_tuple2.typedexpr_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.Tuple3)
    {
       rholang.parsing.lambda.Absyn.Tuple3 _tuple3 = (rholang.parsing.lambda.Absyn.Tuple3) foo;
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

  private static void pp(rholang.parsing.lambda.Absyn.Value foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.VInt)
    {
       rholang.parsing.lambda.Absyn.VInt _vint = (rholang.parsing.lambda.Absyn.VInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.VString)
    {
       rholang.parsing.lambda.Absyn.VString _vstring = (rholang.parsing.lambda.Absyn.VString) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vstring.string_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.lambda.Absyn.Type foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.TSimple)
    {
       rholang.parsing.lambda.Absyn.TSimple _tsimple = (rholang.parsing.lambda.Absyn.TSimple) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_tsimple.simpletype_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.TTuple)
    {
       rholang.parsing.lambda.Absyn.TTuple _ttuple = (rholang.parsing.lambda.Absyn.TTuple) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_ttuple.ttype_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.TFun)
    {
       rholang.parsing.lambda.Absyn.TFun _tfun = (rholang.parsing.lambda.Absyn.TFun) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tfun.type_1, 1);
       render("->");
       pp(_tfun.type_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(rholang.parsing.lambda.Absyn.TType foo, int _i_)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.TType2)
    {
       rholang.parsing.lambda.Absyn.TType2 _ttype2 = (rholang.parsing.lambda.Absyn.TType2) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_ttype2.type_1, 0);
       render(",");
       pp(_ttype2.type_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof rholang.parsing.lambda.Absyn.TType3)
    {
       rholang.parsing.lambda.Absyn.TType3 _ttype3 = (rholang.parsing.lambda.Absyn.TType3) foo;
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


  private static void sh(rholang.parsing.lambda.Absyn.TypedExpr foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.ETyped)
    {
       rholang.parsing.lambda.Absyn.ETyped _etyped = (rholang.parsing.lambda.Absyn.ETyped) foo;
       render("(");
       render("ETyped");
       sh(_etyped.expr_);
       sh(_etyped.type_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.lambda.Absyn.Expr foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.EVar)
    {
       rholang.parsing.lambda.Absyn.EVar _evar = (rholang.parsing.lambda.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.var_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.EVal)
    {
       rholang.parsing.lambda.Absyn.EVal _eval = (rholang.parsing.lambda.Absyn.EVal) foo;
       render("(");
       render("EVal");
       sh(_eval.value_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.EAbs)
    {
       rholang.parsing.lambda.Absyn.EAbs _eabs = (rholang.parsing.lambda.Absyn.EAbs) foo;
       render("(");
       render("EAbs");
       sh(_eabs.var_);
       sh(_eabs.type_);
       sh(_eabs.typedexpr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.EApp)
    {
       rholang.parsing.lambda.Absyn.EApp _eapp = (rholang.parsing.lambda.Absyn.EApp) foo;
       render("(");
       render("EApp");
       sh(_eapp.typedexpr_1);
       sh(_eapp.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.ETuple)
    {
       rholang.parsing.lambda.Absyn.ETuple _etuple = (rholang.parsing.lambda.Absyn.ETuple) foo;
       render("(");
       render("ETuple");
       sh(_etuple.tuple_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.EFirst)
    {
       rholang.parsing.lambda.Absyn.EFirst _efirst = (rholang.parsing.lambda.Absyn.EFirst) foo;
       render("(");
       render("EFirst");
       sh(_efirst.typedexpr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.ESecond)
    {
       rholang.parsing.lambda.Absyn.ESecond _esecond = (rholang.parsing.lambda.Absyn.ESecond) foo;
       render("(");
       render("ESecond");
       sh(_esecond.typedexpr_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.EThird)
    {
       rholang.parsing.lambda.Absyn.EThird _ethird = (rholang.parsing.lambda.Absyn.EThird) foo;
       render("(");
       render("EThird");
       sh(_ethird.typedexpr_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.lambda.Absyn.Tuple foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.Tuple2)
    {
       rholang.parsing.lambda.Absyn.Tuple2 _tuple2 = (rholang.parsing.lambda.Absyn.Tuple2) foo;
       render("(");
       render("Tuple2");
       sh(_tuple2.typedexpr_1);
       sh(_tuple2.typedexpr_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.Tuple3)
    {
       rholang.parsing.lambda.Absyn.Tuple3 _tuple3 = (rholang.parsing.lambda.Absyn.Tuple3) foo;
       render("(");
       render("Tuple3");
       sh(_tuple3.typedexpr_1);
       sh(_tuple3.typedexpr_2);
       sh(_tuple3.typedexpr_3);
       render(")");
    }
  }

  private static void sh(rholang.parsing.lambda.Absyn.Value foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.VInt)
    {
       rholang.parsing.lambda.Absyn.VInt _vint = (rholang.parsing.lambda.Absyn.VInt) foo;
       render("(");
       render("VInt");
       sh(_vint.integer_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.VString)
    {
       rholang.parsing.lambda.Absyn.VString _vstring = (rholang.parsing.lambda.Absyn.VString) foo;
       render("(");
       render("VString");
       sh(_vstring.string_);
       render(")");
    }
  }

  private static void sh(rholang.parsing.lambda.Absyn.Type foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.TSimple)
    {
       rholang.parsing.lambda.Absyn.TSimple _tsimple = (rholang.parsing.lambda.Absyn.TSimple) foo;
       render("(");
       render("TSimple");
       sh(_tsimple.simpletype_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.TTuple)
    {
       rholang.parsing.lambda.Absyn.TTuple _ttuple = (rholang.parsing.lambda.Absyn.TTuple) foo;
       render("(");
       render("TTuple");
       sh(_ttuple.ttype_);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.TFun)
    {
       rholang.parsing.lambda.Absyn.TFun _tfun = (rholang.parsing.lambda.Absyn.TFun) foo;
       render("(");
       render("TFun");
       sh(_tfun.type_1);
       sh(_tfun.type_2);
       render(")");
    }
  }

  private static void sh(rholang.parsing.lambda.Absyn.TType foo)
  {
    if (foo instanceof rholang.parsing.lambda.Absyn.TType2)
    {
       rholang.parsing.lambda.Absyn.TType2 _ttype2 = (rholang.parsing.lambda.Absyn.TType2) foo;
       render("(");
       render("TType2");
       sh(_ttype2.type_1);
       sh(_ttype2.type_2);
       render(")");
    }
    if (foo instanceof rholang.parsing.lambda.Absyn.TType3)
    {
       rholang.parsing.lambda.Absyn.TType3 _ttype3 = (rholang.parsing.lambda.Absyn.TType3) foo;
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

