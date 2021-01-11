package coop.rchain.rholang.interpreter

import java.io.StringReader

import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.rholang.implicits.{GPrivateBuilder, _}
import coop.rchain.models.{Send, _}
import coop.rchain.rholang.interpreter.compiler.{
  BoolNormalizeMatcher,
  DeBruijnLevelMap,
  GroundNormalizeMatcher,
  IndexMapChain,
  NameNormalizeMatcher,
  NameSort,
  NameVisitInputs,
  ProcNormalizeMatcher,
  ProcSort,
  ProcVisitInputs,
  SourcePosition,
  VarSort
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import monix.eval.Coeval
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class BoolPrinterSpec extends FlatSpec with Matchers {

  "GBool(true)" should "Print as \"" + true + "\"" in {
    val btrue = new BoolTrue()
    PrettyPrinter().buildString(BoolNormalizeMatcher.normalizeMatch(btrue)) shouldBe "true"
  }

  "GBool(false)" should "Print as \"" + false + "\"" in {
    val bfalse = new BoolFalse()
    PrettyPrinter().buildString(BoolNormalizeMatcher.normalizeMatch(bfalse)) shouldBe "false"
  }
}

class GroundPrinterSpec extends FlatSpec with Matchers {

  "GroundInt" should "Print as \"" + 7 + "\"" in {
    val gi             = new GroundInt("7")
    val target: String = "7"
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch[Coeval](gi).value) shouldBe target
  }

  "GroundString" should "Print as \"" + "String" + "\"" in {
    val gs             = new GroundString("\"String\"")
    val target: String = "\"" + "String" + "\""
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch[Coeval](gs).value) shouldBe target
  }

  "GroundUri" should "Print with back-ticks" in {
    val gu             = new GroundUri("`Uri`")
    val target: String = "`" + "Uri" + "`"
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch[Coeval](gu).value) shouldBe target
  }
}

class CollectPrinterSpec extends FlatSpec with Matchers {

  val inputs = ProcVisitInputs(
    Par(),
    IndexMapChain
      .empty[VarSort]
      .put(List(("P", ProcSort, SourcePosition(0, 0)), ("x", NameSort, SourcePosition(0, 0)))),
    DeBruijnLevelMap.empty
  )
  implicit val normalizerEnv: Map[String, Par] = Map.empty

  "List" should "Print" in {
    val listData = new ListProc()
    listData.add(new PVar(new ProcVarVar("P")))
    listData.add(new PEval(new NameVar("x")))
    listData.add(new PGround(new GroundInt("7")))
    val list =
      new PCollect(new CollectList(listData, new ProcRemainderVar(new ProcVarVar("ignored"))))

    val result =
      PrettyPrinter(0, 2).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](list, inputs).value.par
      )
    result shouldBe "[x0, x1, 7...free0]"
  }

  "Set" should "Print" in {
    val listData = new ListProc()
    listData.add(new PVar(new ProcVarVar("P")))
    listData.add(new PEval(new NameVar("x")))
    listData.add(new PGround(new GroundInt("7")))
    val list =
      new PCollect(new CollectSet(listData, new ProcRemainderVar(new ProcVarVar("ignored"))))

    val result =
      PrettyPrinter(0, 2).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](list, inputs).value.par
      )
    result shouldBe "Set(7, x1, x0...free0)"
  }

  "Map" should "Print" in {
    val mapData = new ListKeyValuePair()
    mapData.add(
      new KeyValuePairImpl(
        new PGround(new GroundInt("7")),
        new PGround(new GroundString("\"Seven\""))
      )
    )
    mapData.add(new KeyValuePairImpl(new PVar(new ProcVarVar("P")), new PEval(new NameVar("x"))))
    val map = new PCollect(new CollectMap(mapData, new ProcRemainderVar(new ProcVarVar("ignored"))))

    val result =
      PrettyPrinter(0, 2).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](map, inputs).value.par
      )
    result shouldBe "{7 : \"" + "Seven" + "\", x0 : x1...free0}"
  }

  "Map" should "Print commas correctly" in {
    val mapData = new ListKeyValuePair()
    mapData.add(
      new KeyValuePairImpl(
        new PGround(new GroundString("\"c\"")),
        new PGround(new GroundInt("3"))
      )
    )
    mapData.add(
      new KeyValuePairImpl(
        new PGround(new GroundString("\"b\"")),
        new PGround(new GroundInt("2"))
      )
    )
    mapData.add(
      new KeyValuePairImpl(
        new PGround(new GroundString("\"a\"")),
        new PGround(new GroundInt("1"))
      )
    )
    val map = new PCollect(new CollectMap(mapData, new ProcRemainderEmpty()))

    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](map, inputs).value.par
      )
    val target = """{"a" : 1, "b" : 2, "c" : 3}"""
    result shouldBe target
  }
}

class ProcPrinterSpec extends FlatSpec with Matchers {
  val inputs                                   = ProcVisitInputs(Par(), IndexMapChain.empty, DeBruijnLevelMap.empty)
  implicit val normalizerEnv: Map[String, Par] = Map.empty

  "New" should "use 0-based indexing" in {
    val source = Par(news = Seq(New(3, Par())))
    val result = PrettyPrinter().buildString(source)
    val target = "new x0, x1, x2 in {\n  Nil\n}"
    result shouldBe target
  }

  "Par" should "Print" in {
    val source: Par = Par(
      exprs = Seq(GInt(0), GBool(true), GString("2"), GUri("www.3cheese.com")),
      unforgeables = Seq(GPrivateBuilder("4"), GPrivateBuilder("5"))
    )
    val result = PrettyPrinter().buildString(source)
    val target = "0 |\ntrue |\n\"2\" |\n`www.3cheese.com` |\nUnforgeable(0x34) |\nUnforgeable(0x35)"
    result shouldBe target
  }

  "PPlusPlus" should "Print" in {
    val source: Par = Par(
      exprs = Seq(EPlusPlusBody(EPlusPlus(GString("abc"), GString("def"))))
    )
    val result = PrettyPrinter().buildString(source)
    val target = """("abc" ++ "def")"""
    result shouldBe target
  }

  "PMod" should "Print" in {
    val source: Par = Par(
      exprs = Seq(EModBody(EMod(GInt(11), GInt(10))))
    )
    val result = PrettyPrinter().buildString(source)
    val target = """(11 % 10)"""
    result shouldBe target
  }

  "PPercentPercent" should "Print" in {
    val source: Par = Par(
      exprs = Seq(
        EPercentPercentBody(
          EPercentPercent(
            GString("Hello, ${name}"),
            EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("Alice")))))
          )
        )
      )
    )
    val result = PrettyPrinter().buildString(source)
    val target = """("Hello, ${name}" %% {"name" : "Alice"})"""
    result shouldBe target
  }

  "EMinusMinus" should "Print" in {
    val source: Par = Par(
      exprs = Seq(
        EMinusMinusBody(
          EMinusMinus(
            ESetBody(ParSet(List[Par](GInt(1), GInt(2), GInt(3)))),
            ESetBody(ParSet(List[Par](GInt(1), GInt(2))))
          )
        )
      )
    )
    val result = PrettyPrinter().buildString(source)
    val target = "(Set(1, 2, 3) -- Set(1, 2))"
    result shouldBe target
  }

  "Send" should "Print" in {
    val source: Par =
      Par(sends = Seq(Send(Par(), List(Par(), Par()), true, BitSet())))
    val result = PrettyPrinter().buildString(source)
    val target = "@{Nil}!!(Nil, Nil)"
    result shouldBe target
  }

  "Receive" should "print peek" in {
    checkRoundTrip(
      """for( @{x0}, @{x1} <<- @{Nil} ) {
        |  @{x0}!(x1)
        |}""".stripMargin
    )
  }

  "Receive" should "Print variable names consistently" in {
    // new x in { for( z <- x ) { *z } }

    val listBindings = new ListName()
    listBindings.add(new NameVar("z"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val cont         = new PEval(new NameVar("z"))
    val receive      = new PInput(receipt, cont)
    val nameDec      = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x"))
    val source = new PNew(nameDec, receive)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0 in {
        |  for( @{x1} <- x0 ) {
        |    x1
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "Receive" should "Print multiple patterns" in {
    // new x in { for( y, z <- x ) { *y | *z } }

    val listBindings = new ListName()
    listBindings.add(new NameVar("y"))
    listBindings.add(new NameVar("z"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val cont         = new PPar(new PEval(new NameVar("y")), new PEval(new NameVar("z")))
    val receive      = new PInput(receipt, cont)
    val nameDec      = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x"))
    val source = new PNew(nameDec, receive)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0 in {
        |  for( @{x1}, @{x2} <- x0 ) {
        |    x2 |
        |    x1
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "Receive" should "Print multiple binds" in {
    // new x in { for( y <- x ; z <- x ){ *y | *z } }

    val listBindings = new ListName()
    listBindings.add(new NameVar("y"))
    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("z"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x0"))
    )
    listLinearBinds.add(
      new LinearBindImpl(listBindings1, new NameRemainderEmpty(), new NameVar("x1"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val cont         = new PPar(new PEval(new NameVar("y")), new PEval(new NameVar("z")))
    val receive      = new PInput(receipt, cont)
    val nameDec      = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x0"))
    nameDec.add(new NameDeclSimpl("x1"))
    val source = new PNew(nameDec, receive)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0, x1 in {
        |  for( @{x2} <- x1 ; @{x3} <- x0 ) {
        |    x2 |
        |    x3
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "Receive" should "Print multiple binds with multiple patterns" in {
    // new x, y in { for( z, v <- x ; a, b <- y ){ *z | *v | *a | *b }

    val listBindings = new ListName()
    listBindings.add(new NameVar("z"))
    listBindings.add(new NameVar("v"))
    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("a"))
    listBindings1.add(new NameVar("b"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x"))
    )
    listLinearBinds.add(
      new LinearBindImpl(listBindings1, new NameRemainderEmpty(), new NameVar("y"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val cont = new PPar(
      new PPar(new PEval(new NameVar("z")), new PEval(new NameVar("v"))),
      new PPar(new PEval(new NameVar("a")), new PEval(new NameVar("b")))
    )
    val receive = new PInput(receipt, cont)
    val nameDec = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x"))
    nameDec.add(new NameDeclSimpl("y"))
    val source = new PNew(nameDec, receive)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0, x1 in {
        |  for( @{x2}, @{x3} <- x1 ; @{x4}, @{x5} <- x0 ) {
        |    x3 |
        |    x2 |
        |    x5 |
        |    x4
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "Receive" should "Print partially empty Pars" in {
    // new x, y in { for( z, v <- x ; a, b <- y ){ *b!(Nil) | *z | *v | *a }

    val listBindings = new ListName()
    listBindings.add(new NameVar("z"))
    listBindings.add(new NameVar("v"))
    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("a"))
    listBindings1.add(new NameVar("b"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x"))
    )
    listLinearBinds.add(
      new LinearBindImpl(listBindings1, new NameRemainderEmpty(), new NameVar("y"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val sentData     = new ListProc()
    sentData.add(new PNil())
    val pSend = new PSend(new NameVar("b"), new SendSingle(), sentData)
    val cont = new PPar(
      new PPar(new PEval(new NameVar("z")), new PEval(new NameVar("v"))),
      new PPar(new PEval(new NameVar("a")), pSend)
    )
    val receive = new PInput(receipt, cont)
    val nameDec = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x"))
    nameDec.add(new NameDeclSimpl("y"))
    val source = new PNew(nameDec, receive)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0, x1 in {
        |  for( @{x2}, @{x3} <- x1 ; @{x4}, @{x5} <- x0 ) {
        |    @{x3}!(Nil) |
        |    x2 |
        |    x5 |
        |    x4
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "Reducible" should "Print variable names consistently" in {
    // new x in { x!(*x) | for( z <- x ){ *z } }

    val listBindings = new ListName()
    listBindings.add(new NameVar("z"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameVar("x"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val cont         = new PEval(new NameVar("z"))
    val sentData     = new ListProc()
    sentData.add(new PEval(new NameVar("x")))
    val body =
      new PPar(new PSend(new NameVar("x"), new SendSingle(), sentData), new PInput(receipt, cont))
    val nameDec = new ListNameDecl()
    nameDec.add(new NameDeclSimpl("x"))
    val source = new PNew(nameDec, body)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](source, inputs).value.par
      )
    val target =
      """new x0 in {
        |  x0!(*x0) |
        |  for( @{x1} <- x0 ) {
        |    x1
        |  }
        |}""".stripMargin
    result shouldBe target
  }

  "PNil" should "Print" in {
    val nil = new PNil()
    val result = PrettyPrinter().buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](nil, inputs).value.par
    )
    result shouldBe "Nil"
  }

  val pvar = new PVar(new ProcVarVar("x"))
  "PVar" should "Print with fresh identifier" in {
    val boundInputs = inputs.copy(env = inputs.env.put(("x", ProcSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](pvar, boundInputs).value.par
      )
    result shouldBe "x0"
  }

  "PVarRef" should "Print with referenced identifier" in {
    checkRoundTrip(
      """for( @{x0}, @{x1} <- @{0} ) {
        |  match x0 {
        |    =x0 => {
        |      Nil
        |    }
        |    =x1 => {
        |      Nil
        |    }
        |  }
        |}""".stripMargin
    )
  }

  "PEval" should "Print eval with fresh identifier" in {
    val pEval       = new PEval(new NameVar("x"))
    val boundInputs = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](pEval, boundInputs).value.par
      )
    result shouldBe "x0"
  }

  it should "Recognize occurrences of the same variable during collapses" in {
    val pEval = new PEval(
      new NameQuote(new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("x"))))
    )
    val boundInputs = inputs.copy(env = inputs.env.put(("x", ProcSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](pEval, boundInputs).value.par
      )
    result shouldBe
      """x0 |
        |x0""".stripMargin
  }

  it should "Print asterisk for variable introduced by new" in {
    checkRoundTrip(
      """new x0 in {
        |  *x0
        |}""".stripMargin
    )
  }

  it should "Print asterisk for sent name introduced by new" in {
    checkRoundTrip(
      """new x0 in {
        |  @{Nil}!(*x0)
        |}""".stripMargin
    )
  }

  it should "Print asterisk for multiple sent names introduced by new" in {
    checkRoundTrip(
      """new x0, x1 in {
        |  @{0}!(*x1) |
        |  @{1}!(*x0)
        |}""".stripMargin
    )
  }

  it should "Print asterisk for multiple sent names introduced by different news" in {
    checkRoundTrip(
      """new x0 in {
        |  new x1 in {
        |    @{0}!(*x1) |
        |    @{1}!(*x0)
        |  }
        |}""".stripMargin
    )
  }

  "PSend" should "Print" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt("7")))
    sentData.add(new PGround(new GroundInt("8")))
    val pSend = new PSend(new NameQuote(new PNil()), new SendSingle(), sentData)
    val result = PrettyPrinter().buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](pSend, inputs).value.par
    )
    result shouldBe "@{Nil}!(7, 8)"
  }

  "PSend" should "Identify variables as they're bound" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt("7")))
    sentData.add(new PGround(new GroundInt("8")))
    val pSend       = new PSend(new NameVar("x"), new SendSingle(), sentData)
    val boundInputs = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](pSend, boundInputs).value.par
      )
    result shouldBe "@{x0}!(7, 8)"
  }

  "PPar" should "Respect sorting" in {
    val parGround = new PPar(new PGround(new GroundInt("7")), new PGround(new GroundInt("8")))
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](parGround, inputs).value.par
      )
    result shouldBe
      """8 |
        |7""".stripMargin
  }

  "PPar" should "Print" in {
    val parDoubleBound = new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("x")))
    val boundInputs    = inputs.copy(env = inputs.env.put(("x", ProcSort, SourcePosition(0, 0))))
    val result = PrettyPrinter(0, 1).buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](parDoubleBound, boundInputs).value.par
    )
    result shouldBe
      """x0 |
        |x0""".stripMargin
  }

  "PPar" should "Use fresh identifiers for free variables" in {
    val parDoubleFree = new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("y")))
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](parDoubleFree, inputs).value.par
      )
    result shouldBe
      """free1 |
        |free0""".stripMargin
  }

  "PInput" should "Print a receive" in {
    // for ( x, @for( @y, z <- @Nil ){ y | u | *z } <- @Nil ) { x!(u) }

    val listBindings = new ListName()
    listBindings.add(new NameQuote(new PVar(new ProcVarVar("y"))))
    listBindings.add(new NameVar("z"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameQuote(new PNil()))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val body = new PPar(
      new PVar(new ProcVarVar("y")),
      new PPar(new PEval(new NameVar("z")), new PVar(new ProcVarVar("u")))
    )
    val basicInput    = new PInput(receipt, body)
    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("x"))
    listBindings1.add(new NameQuote(basicInput))
    val listLinearBinds1 = new ListLinearBind()
    listLinearBinds1.add(
      new LinearBindImpl(listBindings1, new NameRemainderEmpty(), new NameQuote(new PNil()))
    )
    val linearSimple1 = new LinearSimple(listLinearBinds1)
    val receipt1      = new ReceiptLinear(linearSimple1)
    val listSend1     = new ListProc()
    listSend1.add(new PVar(new ProcVarVar("u")))
    val body1       = new PSend(new NameVar("x"), new SendSingle(), listSend1)
    val basicInput1 = new PInput(receipt1, body1)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](basicInput1, inputs).value.par
      )
    val target =
      """for( @{x0}, @{for( @{y0}, @{y1} <- @{Nil} ) { y1 | y0 | x1 }} <- @{Nil} ) {
        |  @{x0}!(x1)
        |}""".stripMargin
    result shouldBe target
  }

  "PInput" should "Print a more complicated receive" in {
    // new x, y in { for ( z, @a <- y ; b, @c <- x ) { z!(c) | b!(a) | for( d <- b ){ *d | match d { case 42 => Nil case e => c } }

    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("x1"))
    listBindings1.add(new NameQuote(new PVar(new ProcVarVar("y1"))))
    val listBindings2 = new ListName()
    listBindings2.add(new NameVar("x2"))
    listBindings2.add(new NameQuote(new PVar(new ProcVarVar("y2"))))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings1, new NameRemainderEmpty(), new NameVar("x"))
    )
    listLinearBinds.add(
      new LinearBindImpl(listBindings2, new NameRemainderEmpty(), new NameVar("v"))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val listSend1    = new ListProc()
    listSend1.add(new PVar(new ProcVarVar("y2")))
    val listSend2 = new ListProc()
    listSend2.add(new PVar(new ProcVarVar("y1")))
    val listBindings3 = new ListName()
    listBindings3.add(new NameVar("z"))
    val listLinearBinds2 = new ListLinearBind()
    listLinearBinds2.add(
      new LinearBindImpl(listBindings3, new NameRemainderEmpty(), new NameVar("x1"))
    )
    val receipt2 = new ReceiptLinear(new LinearSimple(listLinearBinds2))
    val body = new PPar(
      new PSend(new NameVar("x1"), new SendSingle(), listSend1),
      new PSend(new NameVar("x2"), new SendSingle(), listSend2)
    )
    val listCases = new ListCase()
    listCases.add(new CaseImpl(new PGround(new GroundInt("42")), new PNil()))
    listCases.add(new CaseImpl(new PVar(new ProcVarVar("y")), new PVar(new ProcVarVar("y2"))))
    val body3 = new PPar(
      body,
      new PInput(
        receipt2,
        new PPar(new PEval(new NameVar("z")), new PMatch(new PEval(new NameVar("z")), listCases))
      )
    )
    val listNameDecl = new ListNameDecl()
    listNameDecl.add(new NameDeclSimpl("x"))
    listNameDecl.add(new NameDeclSimpl("v"))
    val pInput = new PNew(listNameDecl, new PInput(receipt, body3))
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](pInput, inputs).value.par
      )
    result shouldBe
      """new x0, x1 in {
        |  for( @{x2}, @{x3} <- x1 ; @{x4}, @{x5} <- x0 ) {
        |    @{x2}!(x5) |
        |    @{x4}!(x3) |
        |    for( @{x6} <- @{x4} ) {
        |      x6 |
        |      match x6 {
        |        42 => {
        |          Nil
        |        }
        |        x7 => {
        |          x3
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
  }

  "PNew" should "Adjust levels of variables as they're bound" in {
    // new x, y, z in { x!(7) | y!(8) | z!(9) }

    val listNameDecl = new ListNameDecl()
    listNameDecl.add(new NameDeclSimpl("x"))
    listNameDecl.add(new NameDeclSimpl("y"))
    listNameDecl.add(new NameDeclSimpl("z"))
    val listData1 = new ListProc()
    listData1.add(new PGround(new GroundInt("7")))
    val listData2 = new ListProc()
    listData2.add(new PGround(new GroundInt("8")))
    val listData3 = new ListProc()
    listData3.add(new PGround(new GroundInt("9")))
    val pNew = new PNew(
      listNameDecl,
      new PPar(
        new PPar(
          new PSend(new NameVar("x"), new SendSingle(), listData1),
          new PSend(new NameVar("y"), new SendSingle(), listData2)
        ),
        new PSend(new NameVar("z"), new SendSingle(), listData3)
      )
    )
    val result = PrettyPrinter()
      .buildString(
        ProcNormalizeMatcher
          .normalizeMatch[Coeval](pNew, inputs)
          .value
          .par
      )
    result shouldBe
      """new x0, x1, x2 in {
        |  x2!(9) |
        |  x1!(8) |
        |  x0!(7)
        |}""".stripMargin
  }

  "PMatch" should "Print recognize pattern bindings" in {
    // for (@x <- @Nil) { match x { 42 => Nil y => Nil } } | @Nil!(47)

    val listBindings = new ListName()
    listBindings.add(new NameQuote(new PVar(new ProcVarVar("x"))))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameQuote(new PNil()))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val listCases    = new ListCase()
    listCases.add(new CaseImpl(new PGround(new GroundInt("42")), new PNil()))
    listCases.add(new CaseImpl(new PVar(new ProcVarVar("y")), new PNil()))
    val body     = new PMatch(new PVar(new ProcVarVar("x")), listCases)
    val listData = new ListProc()
    listData.add(new PGround(new GroundInt("47")))
    val send47OnNil = new PSend(new NameQuote(new PNil()), new SendSingle(), listData)
    val pPar = new PPar(
      new PInput(receipt, body),
      send47OnNil
    )
    val result = PrettyPrinter().buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](pPar, inputs).value.par
    )
    result shouldBe
      """@{Nil}!(47) |
        |for( @{x0} <- @{Nil} ) {
        |  match x0 {
        |    42 => {
        |      Nil
        |    }
        |    x1 => {
        |      Nil
        |    }
        |  }
        |}""".stripMargin
  }

  it should "Print receive in curly brackets" in {
    checkRoundTrip(
      """match Nil {
        |  _ => {
        |    for( @{_} <- @{Nil} ) {
        |      Nil
        |    }
        |  }
        |}""".stripMargin
    )
  }

  "PIf" should "Print as a match" in {
    val condition = new PGround(new GroundBool(new BoolTrue()))
    val listSend  = new ListProc()
    listSend.add(new PGround(new GroundInt("47")))
    val body       = new PSend(new NameQuote(new PNil()), new SendSingle(), listSend)
    val basicInput = new PIf(condition, body)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](basicInput, inputs).value.par
      )
    result shouldBe
      """match true {
        |  true => {
        |    @{Nil}!(47)
        |  }
        |  false => {
        |    Nil
        |  }
        |}""".stripMargin
  }

  "PIfElse" should "Print" in {
    // if (47 == 47) { new x in { x!(47) } } else { new y in { y!(47) } }
    val condition = new PEq(new PGround(new GroundInt("47")), new PGround(new GroundInt("47")))
    val xNameDecl = new ListNameDecl()
    xNameDecl.add(new NameDeclSimpl("x"))
    val xSendData = new ListProc()
    xSendData.add(new PGround(new GroundInt("47")))
    val pNewIf = new PNew(
      xNameDecl,
      new PSend(new NameVar("x"), new SendSingle(), xSendData)
    )
    val yNameDecl = new ListNameDecl()
    yNameDecl.add(new NameDeclSimpl("y"))
    val ySendData = new ListProc()
    ySendData.add(new PGround(new GroundInt("47")))
    val pNewElse = new PNew(
      yNameDecl,
      new PSend(new NameVar("y"), new SendSingle(), ySendData)
    )
    val basicInput = new PIfElse(condition, pNewIf, pNewElse)
    val result =
      PrettyPrinter().buildString(
        ProcNormalizeMatcher.normalizeMatch[Coeval](basicInput, inputs).value.par
      )
    result shouldBe
      """match (47 == 47) {
        |  true => {
        |    new x0 in {
        |      x0!(47)
        |    }
        |  }
        |  false => {
        |    new x0 in {
        |      x0!(47)
        |    }
        |  }
        |}""".stripMargin
  }

  "PMatch" should "Print" in {
    // for (@{match {x | y} { 47 => Nil }} <- @Nil) { Nil }
    val listCases = new ListCase()
    listCases.add(new CaseImpl(new PGround(new GroundInt("47")), new PNil()))
    val pMatch =
      new PMatch(new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("y"))), listCases)
    val listBindings = new ListName()
    listBindings.add(new NameQuote(pMatch))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(
      new LinearBindImpl(listBindings, new NameRemainderEmpty(), new NameQuote(new PNil()))
    )
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt      = new ReceiptLinear(linearSimple)
    val input        = new PInput(receipt, new PNil())
    val result = PrettyPrinter().buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](input, inputs).value.par
    )
    result shouldBe """for( @{match x0 | x1 { 47 => { Nil } }} <- @{Nil} ) {
                      |  Nil
                      |}""".stripMargin
  }

  "PMatches" should "display matches" in {
    val pMatches = new PMatches(new PGround(new GroundInt("1")), new PVar(new ProcVarWildcard()))

    val result = PrettyPrinter(0, 1).buildString(
      ProcNormalizeMatcher.normalizeMatch[Coeval](pMatches, inputs).value.par
    )

    result shouldBe "(1 matches _)"
  }

  private def checkRoundTrip(prettySource: String): Assertion =
    assert(parseAndPrint(prettySource) == prettySource)

  private def parseAndPrint(source: String): String = PrettyPrinter().buildString(
    ParBuilderUtil.buildNormalizedTerm[Coeval](new StringReader(source)).runAttempt().right.get
  )
}

class IncrementTester extends FlatSpec with Matchers {

  val printer = PrettyPrinter()

  "Increment" should "increment the id prefix every 26 increments" in {
    val id: String = (0 until 26).foldLeft("a") { (s, _) =>
      printer.increment(s)
    }
    val _id: String = (0 until 26).foldLeft(id) { (s, _) =>
      printer.increment(s)
    }
    id shouldBe "aa"
    _id shouldBe "ba"
  }

  "Increment and Rotate" should "" in {

    val _printer: PrettyPrinter = (0 until 52).foldLeft(printer) { (p, _) =>
      p.copy(
        freeId = p.boundId,
        baseId = p.setBaseId()
      )
    }
    _printer.freeId shouldBe "xw"
    _printer.boundId shouldBe "yx"
    _printer.baseId shouldBe "ba"
  }
}

class NamePrinterSpec extends FlatSpec with Matchers {

  val inputs                                   = NameVisitInputs(IndexMapChain.empty, DeBruijnLevelMap.empty)
  implicit val normalizerEnv: Map[String, Par] = Map.empty

  "NameWildcard" should "Print" in {
    val nw = new NameWildcard()
    val result = PrettyPrinter().buildString(
      NameNormalizeMatcher.normalizeMatch[Coeval](nw, inputs).value.chan
    )
    result shouldBe "_"
  }

  val nvar = new NameVar("x")

  "NameVar" should "Print" in {
    val boundInputs = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        NameNormalizeMatcher.normalizeMatch[Coeval](nvar, boundInputs).value.chan
      )
    result shouldBe "x0"
  }

  val nqvar = new NameQuote(new PVar(new ProcVarVar("x")))

  "NameQuote" should "Print" in {
    val nqeval      = new NameQuote(new PPar(new PEval(new NameVar("x")), new PEval(new NameVar("x"))))
    val boundInputs = inputs.copy(env = inputs.env.put(("x", NameSort, SourcePosition(0, 0))))
    val result =
      PrettyPrinter(0, 1).buildString(
        NameNormalizeMatcher.normalizeMatch[Coeval](nqeval, boundInputs).value.chan
      )
    result shouldBe "x0 |\nx0"
  }

}
