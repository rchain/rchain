package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models.{Send, _}
import coop.rchain.rholang.interpreter.implicits.{GPrivate, _}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn._
import org.scalatest.{FlatSpec, Matchers}

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
    val gi = new GroundInt(7)
    val target: String = "7"
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch(gi)) shouldBe target
  }

  "GroundString" should "Print as \"" + "String" + "\"" in {
    val gs = new GroundString("String")
    val target: String = "\"" + "String" + "\""
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch(gs)) shouldBe target
  }

  "GroundUri" should "Print with back-ticks" in {
    val gu = new GroundUri("Uri")
    val target: String = "`" + "Uri" + "`"
    PrettyPrinter().buildString(GroundNormalizeMatcher.normalizeMatch(gu)) shouldBe target
  }
}

class CollectPrinterSpec extends FlatSpec with Matchers {

  val inputs = ProcVisitInputs(
    Par(),
    DebruijnIndexMap[VarSort]().newBindings(List(("P", ProcSort, 0, 0), ("x", NameSort, 0, 0))),
    DebruijnLevelMap[VarSort]())

  "List" should "Print" in {
    val listData = new ListProc()
    listData.add(new PVar(new ProcVarVar("P")))
    listData.add(new PEval(new NameVar("x")))
    listData.add(new PGround(new GroundInt(7)))
    val list = new PCollect(new CollectList(listData))

    val result =
      PrettyPrinter(0, 2).buildString(ProcNormalizeMatcher.normalizeMatch(list, inputs).par)
    val target = "[x0, *x1, 7]"
    result shouldBe target
  }

  "Map" should "Print" in {
    val mapData = new ListKeyValuePair()
    mapData.add(
      new KeyValuePairImpl(new PGround(new GroundInt(7)), new PGround(new GroundString("Seven"))))
    mapData.add(new KeyValuePairImpl(new PVar(new ProcVarVar("P")), new PEval(new NameVar("x"))))
    val map = new PCollect(new CollectMap(mapData))

    val result =
      PrettyPrinter(0, 2).buildString(ProcNormalizeMatcher.normalizeMatch(map, inputs).par)
    result shouldBe "{7 : \"" + "Seven" + "\", x0 : *x1}"
  }

}

class ProcPrinterSpec extends FlatSpec with Matchers {

  val inputs = ProcVisitInputs(Par(), DebruijnIndexMap[VarSort](), DebruijnLevelMap[VarSort]())

  val p: Par = Par()

  "New" should "use 0-based indexing" in {
    val source = p.copy(news = Seq(New(3, Par())))
    val result = PrettyPrinter().buildString(source)
    val target = "new x0, x1, x2 in { Nil }"
    result shouldBe target
  }

  "Par" should "Print" in {
    val source: Par = p.copy(
      exprs = Seq(GInt(0), GBool(true), GString("2"), GUri("www.3cheese.com")),
      ids = Seq(GPrivate("4"), GPrivate("5"))
    )
    val result = PrettyPrinter().buildString(source)
    val target = "0 | true | \"2\" | `www.3cheese.com` | 4 | 5"
    result shouldBe target
  }

  "Send" should "Print" in {
    val source: Par =
      p.copy(sends = Seq(Send(Quote(Par()), List(Par(), Par()), true, 0, BitSet())))
    val result = PrettyPrinter().buildString(source)
    val target = "@{Nil}!!(Nil, Nil)"
    result shouldBe target
  }

  "Receive" should "Print variable names consistently" in {

    // new x0 in { for( z0 <- x0 ) { *x0 } }
    val source = p.copy(
      news = Seq(
        New(1,
            p.copy(
              receives = Seq(
                Receive(
                  Seq(
                    ReceiveBind(
                      Seq(ChanVar(FreeVar(0))),
                      ChanVar(BoundVar(0))
                    )
                  ),
                  p.copy(evals = Seq(Eval(ChanVar(BoundVar(0)))))
                )
              )
            ))))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( x1 <- x0 ) { *x1 } }"
    result shouldBe target
  }

  "Receive" should "Print multiple patterns" in {

    // new x0 in { for( z0, z1 <- x0 ) { *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(1))), Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( x1, x2 <- x0 ) { *x1 | *x2 } }"
    result shouldBe target
  }

  "Receive" should "Print multiple binds" in {
    // new x0 in { for( z0 <- x0 ; z0 <- x0 ) { *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0))),
                  ChanVar(BoundVar(0))
                ),
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(1))), Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( x1 <- x0 ; x2 <- x0 ) { *x1 | *x2 } }"
    result shouldBe target
  }

  "Receive" should "Print multiple binds with multiple patterns" in {

    // new x0, x1 in { for( z0, z1 <- x0 ; z0, z1 <- x1 ){ *x3 | *x2 | *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        2,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(1))
                ),
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(3))),
                                 Eval(ChanVar(BoundVar(2))),
                                 Eval(ChanVar(BoundVar(1))),
                                 Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0, x1 in { for( x2, x3 <- x0 ; x4, x5 <- x1 ) { *x2 | *x3 | *x4 | *x5 } }"
    result shouldBe target
  }

  "Receive" should "Print partially empty Pars" in {
    val source = p.copy(
      news = Seq(New(
        2,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(1))
                ),
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(
                evals = Seq(
                  Eval(ChanVar(BoundVar(3))),
                  Eval(ChanVar(BoundVar(2))),
                  Eval(ChanVar(BoundVar(1)))
                ),
                sends = Seq(
                  Send(ChanVar(BoundVar(0)), List(Par()), false)
                )
              )
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target =
      "new x0, x1 in { for( x2, x3 <- x0 ; x4, x5 <- x1 ) { x5!(Nil) | *x2 | *x3 | *x4 } }"
    result shouldBe target
  }

  "Reducible" should "Print variable names consistently" in {
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(ReceiveBind(
                Seq(ChanVar(FreeVar(0))),
                ChanVar(BoundVar(0))
              )),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(0)))))
            )
          ),
          sends = Seq(
            Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet())
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { x0!(Nil) | for( x1 <- x0 ) { *x1 } }"
    result shouldBe target
  }

  "PNil" should "Print" in {
    val nil = new PNil()
    val result = PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(nil, inputs).par)
    result shouldBe "Nil"
  }

  val pvar = new PVar(new ProcVarVar("x"))
  "PVar" should "Print with fresh identifier" in {
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", ProcSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(ProcNormalizeMatcher.normalizeMatch(pvar, boundInputs).par)
    result shouldBe "x0"
  }

  "PEval" should "Print eval with fresh identifier" in {
    val pEval = new PEval(new NameVar("x"))
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", NameSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(ProcNormalizeMatcher.normalizeMatch(pEval, boundInputs).par)
    result shouldBe "*x0"
  }

  "PEval" should "Recognize occurrences of the same variable during collapses" in {
    val pEval = new PEval(
      new NameQuote(new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("x")))))
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", ProcSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(ProcNormalizeMatcher.normalizeMatch(pEval, boundInputs).par)
    result shouldBe "x0 | x0"
  }

  "PSend" should "Print" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt(7)))
    sentData.add(new PGround(new GroundInt(8)))
    val pSend = new PSend(new NameQuote(new PNil()), new SendSingle(), sentData)

    val result = PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(pSend, inputs).par)
    result shouldBe "@{Nil}!(7, 8)"
  }

  "PSend" should "Identify variables as they're bound" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt(7)))
    sentData.add(new PGround(new GroundInt(8)))
    val pSend = new PSend(new NameVar("x"), new SendSingle(), sentData)
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", NameSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(ProcNormalizeMatcher.normalizeMatch(pSend, boundInputs).par)
    result shouldBe "x0!(7, 8)"
  }

  "PPar" should "Respect sorting" in {
    val parGround = new PPar(new PGround(new GroundInt(7)), new PGround(new GroundInt(8)))
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(parGround, inputs).par)
    result shouldBe "8 | 7"
  }

  "PPar" should "Print" in {
    val parDoubleBound = new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("x")))
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", ProcSort, 0, 0)))
    val result = PrettyPrinter(0, 1).buildString(
      ProcNormalizeMatcher.normalizeMatch(parDoubleBound, boundInputs).par)
    result shouldBe "x0 | x0"
  }

  "PPar" should "Use fresh identifiers for free variables" in {
    val parDoubleFree = new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("y")))
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(parDoubleFree, inputs).par)
    result shouldBe "INVALID1 | INVALID0"
  }

  "PInput" should "Print a receive" in {
    // for ( x, y <- @Nil ) { x!(*y) }
    val listBindings = new ListName()
    listBindings.add(new NameVar("x"))
    listBindings.add(new NameVar("y"))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(new LinearBindImpl(listBindings, new NameQuote(new PNil())))
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt = new ReceiptLinear(linearSimple)
    val listSend = new ListProc()
    listSend.add(new PEval(new NameVar("y")))
    val body = new PSend(new NameVar("x"), new SendSingle(), listSend)
    val basicInput = new PInput(receipt, body)
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(basicInput, inputs).par)
    val target = "for( x0, x1 <- @{Nil} ) { x0!(*x1) }"
    result shouldBe target
  }

  "PInput" should "Print a more complicated receive" in {
    // for ( (x1, @y1) <- @Nil ; (x2, @y2) <- @1) { x1!(y2) | x2!(y1) }
    val listBindings1 = new ListName()
    listBindings1.add(new NameVar("x1"))
    listBindings1.add(new NameQuote(new PVar(new ProcVarVar("y1"))))
    val listBindings2 = new ListName()
    listBindings2.add(new NameVar("x2"))
    listBindings2.add(new NameQuote(new PVar(new ProcVarVar("y2"))))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(new LinearBindImpl(listBindings1, new NameQuote(new PNil())))
    listLinearBinds.add(
      new LinearBindImpl(listBindings2, new NameQuote(new PGround(new GroundInt(1)))))
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt = new ReceiptLinear(linearSimple)

    val listSend1 = new ListProc()
    listSend1.add(new PVar(new ProcVarVar("y2")))
    val listSend2 = new ListProc()
    listSend2.add(new PVar(new ProcVarVar("y1")))
    val body = new PPar(new PSend(new NameVar("x1"), new SendSingle(), listSend1),
      new PSend(new NameVar("x2"), new SendSingle(), listSend2))
    val pInput = new PInput(receipt, body)
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(pInput, inputs).par)
    result shouldBe "for( x0, @{x1} <- @{Nil} ; x2, @{x3} <- @{1} ) { x2!(x1) | x0!(x3) }"
  }

  "PNew" should "Adjust levels of variables as they're bound" in {
    val listNameDecl = new ListNameDecl()
    listNameDecl.add(new NameDeclSimpl("x"))
    listNameDecl.add(new NameDeclSimpl("y"))
    listNameDecl.add(new NameDeclSimpl("z"))
    val listData1 = new ListProc()
    listData1.add(new PGround(new GroundInt(7)))
    val listData2 = new ListProc()
    listData2.add(new PGround(new GroundInt(8)))
    val listData3 = new ListProc()
    listData3.add(new PGround(new GroundInt(9)))
    val pNew = new PNew(
      listNameDecl,
      new PPar(
        new PPar(new PSend(new NameVar("x"), new SendSingle(), listData1),
          new PSend(new NameVar("y"), new SendSingle(), listData2)),
        new PSend(new NameVar("z"), new SendSingle(), listData3)
      )
    )
    val result = PrettyPrinter()
      .buildString(
        ProcNormalizeMatcher
          .normalizeMatch(pNew, inputs)
          .par)
    result shouldBe "new x0, x1, x2 in { x2!(9) | x1!(8) | x0!(7) }"
  }

  "PMatch" should "Print recognize pattern bindings" in {
    // for (@x <- @Nil) { match x { case 42 => Nil ; case y => Nil } | @Nil!(47)
    val listBindings = new ListName()
    listBindings.add(new NameQuote(new PVar(new ProcVarVar("x"))))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(new LinearBindImpl(listBindings, new NameQuote(new PNil())))
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt = new ReceiptLinear(linearSimple)
    val listCases = new ListCase()
    listCases.add(new CaseImpl(new PGround(new GroundInt(42)), new PNil()))
    listCases.add(new CaseImpl(new PVar(new ProcVarVar("y")), new PNil()))
    val body = new PMatch(new PVar(new ProcVarVar("x")), listCases)
    val listData = new ListProc()
    listData.add(new PGround(new GroundInt(47)))
    val send47OnNil = new PSend(new NameQuote(new PNil()), new SendSingle(), listData)
    val pPar = new PPar(
      new PInput(receipt, body),
      send47OnNil
    )
    val result = PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(pPar, inputs).par)
    result shouldBe "@{Nil}!(47) | for( @{x0} <- @{Nil} ) { match x0 { case 42 => Nil ; case x1 => Nil } }"
  }

  "PIf" should "Print as a match" in {
    val condition = new PGround(new GroundBool(new BoolTrue()))
    val listSend = new ListProc()
    listSend.add(new PGround(new GroundInt(47)))
    val body = new PSend(new NameQuote(new PNil()), new SendSingle(), listSend)
    val basicInput = new PIf(condition, body)
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(basicInput, inputs).par)
    result shouldBe "match true { case true => @{Nil}!(47) ; case false => Nil }"
  }

  "PIfElse" should "Print" in {
    // if (47 == 47) { new x in { x!(47) } } else { new y in { y!(47) } }
    val condition = new PEq(new PGround(new GroundInt(47)), new PGround(new GroundInt(47)))
    val xNameDecl = new ListNameDecl()
    xNameDecl.add(new NameDeclSimpl("x"))
    val xSendData = new ListProc()
    xSendData.add(new PGround(new GroundInt(47)))
    val pNewIf = new PNew(
      xNameDecl,
      new PSend(new NameVar("x"), new SendSingle(), xSendData)
    )
    val yNameDecl = new ListNameDecl()
    yNameDecl.add(new NameDeclSimpl("y"))
    val ySendData = new ListProc()
    ySendData.add(new PGround(new GroundInt(47)))
    val pNewElse = new PNew(
      yNameDecl,
      new PSend(new NameVar("y"), new SendSingle(), ySendData)
    )
    val basicInput = new PIfElse(condition, pNewIf, pNewElse)
    val result =
      PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(basicInput, inputs).par)
    result shouldBe "match 47 == 47 { case true => new x0 in { x0!(47) } ; case false => new x0 in { x0!(47) } }"
  }

  "PMatch" should "Print" in {
    // for (@{match {x | y} { 47 => Nil }} <- @Nil) { Nil }
    val listCases = new ListCase()
    listCases.add(new CaseImpl(new PGround(new GroundInt(47)), new PNil()))
    val pMatch =
      new PMatch(new PPar(new PVar(new ProcVarVar("x")), new PVar(new ProcVarVar("y"))), listCases)
    val listBindings = new ListName()
    listBindings.add(new NameQuote(pMatch))
    val listLinearBinds = new ListLinearBind()
    listLinearBinds.add(new LinearBindImpl(listBindings, new NameQuote(new PNil())))
    val linearSimple = new LinearSimple(listLinearBinds)
    val receipt = new ReceiptLinear(linearSimple)
    val input = new PInput(receipt, new PNil())
    val result = PrettyPrinter().buildString(ProcNormalizeMatcher.normalizeMatch(input, inputs).par)
    result shouldBe "for( @{match x0 | x1 { case 47 => Nil }} <- @{Nil} ) { Nil }"
  }

}

class IncrementTester extends FlatSpec with Matchers {

  val printer = PrettyPrinter()

  "Increment" should "increment the id prefix every 26 increments" in {
    val id: String = ("a" /: (0 until 26)) { (s, _) =>
      printer.increment(s)
    }
    val _id: String = (id /: (0 until 26)) { (s, _) =>
      printer.increment(s)
    }
    id shouldBe "aa"
    _id shouldBe "ba"
  }

  "Increment and Rotate" should "" in {
    val _printer: PrettyPrinter = (printer /: (0 until 52)) { (p, _) =>
      p.copy(
        freeId = p.boundId,
        boundId = p.rotate(p.increment(p.baseId)),
        baseId = p.increment(p.baseId)
      )
    }
    _printer.freeId shouldBe "xw"
    _printer.boundId shouldBe "yx"
    _printer.baseId shouldBe "ba"
  }
}

class NamePrinterSpec extends FlatSpec with Matchers {

  val inputs = NameVisitInputs(DebruijnIndexMap[VarSort](), DebruijnLevelMap[VarSort]())

  "NameWildcard" should "Print" in {
    val nw = new NameWildcard()
    val result = PrettyPrinter().buildString(NameNormalizeMatcher.normalizeMatch(nw, inputs).chan)
    result shouldBe "_"
  }

  val nvar = new NameVar("x")

  "NameVar" should "Print" in {
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", NameSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(NameNormalizeMatcher.normalizeMatch(nvar, boundInputs).chan)
    result shouldBe "x0"
  }

  val nqvar = new NameQuote(new PVar(new ProcVarVar("x")))

  "NameQuote" should "Print" in {
    val nqeval = new NameQuote(new PPar(new PEval(new NameVar("x")), new PEval(new NameVar("x"))))
    val boundInputs = inputs.copy(env = inputs.env.newBinding(("x", NameSort, 0, 0)))
    val result =
      PrettyPrinter(0, 1).buildString(NameNormalizeMatcher.normalizeMatch(nqeval, boundInputs).chan)
    result shouldBe "@{*x0 | *x0}"
  }

}
