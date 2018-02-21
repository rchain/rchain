package coop.rchain.rholang.intepreter

import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}
import org.scalatest._

class BoolMatcherSpec extends FlatSpec with Matchers {
  val matcher = BoolNormalizeMatcher
  "BoolTrue" should "Compile as GBool(true)" in {
    val btrue = new BoolTrue()

    matcher.normalizeMatch(btrue) should be (GBool(true))
  }
  "BoolFalse" should "Compile as GBool(false)" in {
    val bfalse = new BoolFalse()

    matcher.normalizeMatch(bfalse) should be (GBool(false))
  }
}

class GroundMatcherSpec extends FlatSpec with Matchers {
  val matcher = GroundNormalizeMatcher
  "GroundInt" should "Compile as GInt" in {
    val gi = new GroundInt(7)

    matcher.normalizeMatch(gi) should be (GInt(7))
  }
  "GroundString" should "Compile as GString" in {
    val gs = new GroundString("String")

    matcher.normalizeMatch(gs) should be (GString("String"))
  }
  "GroundUri" should "Compile as GUri" in {
    val gu = new GroundUri("Uri")

    matcher.normalizeMatch(gu) should be (GUri("Uri"))
  }
}

class ProcMatcherSpec extends FlatSpec with Matchers {
  val inputs = ProcVisitInputs(
      Par(),
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())

  "PNil" should "Compile as no modification to the par object" in {
    val nil = new PNil()

    val result = ProcNormalizeMatcher.normalizeMatch(nil, inputs)
    result.par should be (inputs.par)
    result.knownFree should be (inputs.knownFree)
  }

  val pvar = new PVar("x")
  "PVar" should "Compile as BoundVar if it's in env" in {
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)
  
    val result = ProcNormalizeMatcher.normalizeMatch(pvar, boundInputs)
    result.par should be (inputs.par.copy(exprs = List(EVar(BoundVar(0)))))
    result.knownFree should be (inputs.knownFree)
  }
  "PVar" should "Compile as FreeVar if it's not in env" in {
    val result = ProcNormalizeMatcher.normalizeMatch(pvar, inputs)
    result.par should be (inputs.par.copy(exprs = List(EVar(FreeVar(0)))))
    result.knownFree shouldEqual
        (inputs.knownFree.newBindings(
            List((Some("x"), ProcSort)))._1)
  }
  "PVar" should "Not compile if it's in env of the wrong sort" in {
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)
    
    an [Error] should be thrownBy {
      ProcNormalizeMatcher.normalizeMatch(pvar, boundInputs)
    }
  }
  "PVar" should "Not compile if it's used free somewhere else" in {
    val boundInputs = inputs.copy(knownFree =
      inputs.knownFree.newBindings(List((Some("x"), ProcSort)))._1)
    
    an [Error] should be thrownBy {
      ProcNormalizeMatcher.normalizeMatch(pvar, boundInputs)
    }
  }

  "PEval" should "Handle a bound name varible" in {
    val pEval = new PEval(new NameVar("x"))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pEval, boundInputs)
    result.par should be (inputs.par.copy(evals = List(Eval(ChanVar(BoundVar(0))))))
    result.knownFree should be (inputs.knownFree)
  }
  "PEval" should "Collapse a quote" in {
    val pEval = new PEval(new NameQuote(new PPar(new PVar("x"), new PVar("x"))))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pEval, boundInputs)
    result.par should be (inputs.par.copy(exprs = List(EVar(BoundVar(0)), EVar(BoundVar(0)))))
    result.knownFree should be (inputs.knownFree)
  }

  "PNot" should "Delegate" in {
    val pNot = new PNot(new PGround(new GroundBool(new BoolFalse())))

    val result = ProcNormalizeMatcher.normalizeMatch(pNot, inputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(ENot(Par().copy(exprs = List(GBool(false)))))))
    result.knownFree should be (inputs.knownFree)
  }

  "PNeg" should "Delegate" in {
    val pNeg = new PNeg(new PVar("x"))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pNeg, boundInputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(ENeg(Par().copy(exprs = List(EVar(BoundVar(0))))))))
    result.knownFree should be (inputs.knownFree)
  }

  "PMult" should "Delegate" in {
    val pMult = new PMult(new PVar("x"), new PVar("y"))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pMult, boundInputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(EMult(
                Par().copy(exprs = List(EVar(BoundVar(0)))),
                Par().copy(exprs = List(EVar(FreeVar(0))))))))
    result.knownFree should be (inputs.knownFree.newBindings(List((Some("y"), ProcSort)))._1)
  }

  "PDiv" should "Delegate" in {
    val pDiv = new PDiv(new PGround(new GroundInt(7)), new PGround(new GroundInt(2)))

    val result = ProcNormalizeMatcher.normalizeMatch(pDiv, inputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(EDiv(
                Par().copy(exprs = List(GInt(7))),
                Par().copy(exprs = List(GInt(2)))))))
    result.knownFree should be (inputs.knownFree)
  }

  "PAdd" should "Delegate" in {
    val pAdd = new PAdd(new PVar("x"), new PVar("y"))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort), (Some("y"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pAdd, boundInputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(EPlus(
                Par().copy(exprs = List(EVar(BoundVar(0)))),
                Par().copy(exprs = List(EVar(BoundVar(1))))))))
    result.knownFree should be (inputs.knownFree)
  }

  "PMinus" should "Delegate" in {
    val pMinus = new PMinus(new PVar("x"), new PMult(new PVar("y"), new PVar("z")))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort),
                                  (Some("y"), ProcSort),
                                  (Some("z"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pMinus, boundInputs)
    result.par should be (
        inputs.par.copy(
            exprs = List(EMinus(
                Par().copy(exprs = List(EVar(BoundVar(0)))),
                Par().copy(exprs = List(EMult(
                    Par().copy(exprs = List(EVar(BoundVar(1)))),
                    Par().copy(exprs = List(EVar(BoundVar(2)))))))))))
    result.knownFree should be (inputs.knownFree)
  }

  "PSend" should "handle a basic send" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt(7)))
    sentData.add(new PGround(new GroundInt(8)))
    val pSend = new PSend(new NameQuote(new PNil()), new SendSingle(), sentData)

    val result = ProcNormalizeMatcher.normalizeMatch(pSend, inputs)
    result.par should be (
        inputs.par.copy(
            sends = List(Send(
                Quote(Par()),
                List(Par().copy(exprs = List(GInt(7))),
                     Par().copy(exprs = List(GInt(8)))),
                false))))
    result.knownFree should be (inputs.knownFree)
  }

  "PSend" should "handle a name var" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt(7)))
    sentData.add(new PGround(new GroundInt(8)))
    val pSend = new PSend(new NameVar("x"), new SendSingle(), sentData)
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pSend, boundInputs)
    result.par should be (
        inputs.par.copy(
            sends = List(Send(
                ChanVar(BoundVar(0)),
                List(Par().copy(exprs = List(GInt(7))),
                     Par().copy(exprs = List(GInt(8)))),
                false))))
    result.knownFree should be (inputs.knownFree)
  }

  "PSend" should "propagate knownFree" in {
    val sentData = new ListProc()
    sentData.add(new PGround(new GroundInt(7)))
    sentData.add(new PVar("x"))
    val pSend = new PSend(new NameQuote(new PVar("x")), new SendSingle(), sentData)

    an [Error] should be thrownBy {
      ProcNormalizeMatcher.normalizeMatch(pSend, inputs)
    }
  }

  "PPar" should "Compile both branches into a par object" in {
    val parGround = new PPar(
        new PGround(
            new GroundInt(7)),
        new PGround(
            new GroundInt(8)))
    val result = ProcNormalizeMatcher.normalizeMatch(parGround, inputs)
    result.par should be (
        inputs.par.copy(exprs =
            List(GInt(8), GInt(7))))
    result.knownFree should be (inputs.knownFree)
  }

  "PPar" should "Compile both branches with the same environment" in {
    val parDoubleBound = new PPar(
        new PVar("x"),
        new PVar("x"))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(parDoubleBound, boundInputs)
    result.par should be (
        inputs.par.copy(exprs =
            List(EVar(BoundVar(0)), EVar(BoundVar(0)))))
    result.knownFree should be (inputs.knownFree)
  }
  "PPar" should "Not compile if both branches use the same free variable" in {
    val parDoubleFree = new PPar(
        new PVar("x"),
        new PVar("x"))
    an [Error] should be thrownBy {
      ProcNormalizeMatcher.normalizeMatch(parDoubleFree, inputs)
    }
  }
  "PContr" should "Handle a basic contract" in {
    /*  new add in {
          contract add(ret, @x, @y) = {
            ret!(x + y)
          }
        }
        // new is simulated by bindings.
    */
    val listBindings = new ListName()
    listBindings.add(new NameVar("ret"))
    listBindings.add(new NameQuote(new PVar("x")))
    listBindings.add(new NameQuote(new PVar("y")))
    val freeCount = 3
    val listSend = new ListProc()
    listSend.add(new PAdd(new PVar("x"), new PVar("y")))
    val pBasicContr = new PContr(new NameVar("add"), listBindings,
      new PSend(new NameVar("ret"), new SendSingle(), listSend))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("add"), NameSort)))._1)
    
    val result = ProcNormalizeMatcher.normalizeMatch(pBasicContr, boundInputs)
    result.par should be (
        inputs.par.copy(receives = 
            List(Receive(
                List(
                    (List(
                        ChanVar(FreeVar(0)),
                        Quote(Par().copy(exprs = List(EVar(FreeVar(1))))),
                        Quote(Par().copy(exprs = List(EVar(FreeVar(2)))))),
                    ChanVar(BoundVar(0)))),
                Par().copy(sends = List(Send(
                    ChanVar(BoundVar(1)),
                    List(Par().copy(exprs = List(EPlus(
                        Par().copy(exprs = List(EVar(BoundVar(2)))),
                        Par().copy(exprs = List(EVar(BoundVar(3)))))))),
                    false))),
                true, // persistent
                freeCount))))
    result.knownFree should be (inputs.knownFree)
  }
  "PContr" should "Not count ground values in the formals towards the free count" in {
    /*  new ret5 in {
          contract ret5(ret, @5) = {
            ret!(5)
          }
        }
        // new is simulated by bindings.
    */
    val listBindings = new ListName()
    listBindings.add(new NameVar("ret"))
    listBindings.add(new NameQuote(new PGround(new GroundInt(5))))
    val freeCount = 1
    val listSend = new ListProc()
    listSend.add(new PGround(new GroundInt(5)))
    val pBasicContr = new PContr(new NameVar("ret5"), listBindings,
      new PSend(new NameVar("ret"), new SendSingle(), listSend))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("ret5"), NameSort)))._1)

    val result = ProcNormalizeMatcher.normalizeMatch(pBasicContr, boundInputs)
    result.par should be (
      inputs.par.copy(receives =
        List(Receive(
          List(
            (List(
              ChanVar(FreeVar(0)),
              Quote(Par().copy(exprs = List(GInt(5))))),
              ChanVar(BoundVar(0)))),
          Par().copy(sends = List(Send(
            ChanVar(BoundVar(1)),
            List(Par().copy(exprs = List(GInt(5)))),
            false))),
          true, // persistent
          freeCount))))
    result.knownFree should be (inputs.knownFree)
  }
}

class NameMatcherSpec extends FlatSpec with Matchers {
  val inputs = NameVisitInputs(
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())

  "NameWildcard" should "Set wildcard flag in knownFree" in {
    val nw = new NameWildcard()
    val result = NameNormalizeMatcher.normalizeMatch(nw, inputs)
    result.chan should be (ChanVar(FreeVar(0)))
    result.knownFree shouldEqual (inputs.knownFree.setWildcardUsed(1)._1)
  }

  val nvar = new NameVar("x")

  "NameVar" should "Compile as BoundVar if it's in env" in {
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)
  
    val result = NameNormalizeMatcher.normalizeMatch(nvar, boundInputs)
    result.chan should be (ChanVar(BoundVar(0)))
    result.knownFree should be (inputs.knownFree)
  }
  "NameVar" should "Compile as FreeVar if it's not in env" in {
    val result = NameNormalizeMatcher.normalizeMatch(nvar, inputs)
    result.chan should be (ChanVar(FreeVar(0)))
    result.knownFree shouldEqual
        (inputs.knownFree.newBindings(
            List((Some("x"), NameSort)))._1)
  }
  "NameVar" should "Not compile if it's in env of the wrong sort" in {
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)
    
    an [Error] should be thrownBy {
      NameNormalizeMatcher.normalizeMatch(nvar, boundInputs)
    }
  }
  "NameVar" should "Not compile if it's used free somewhere else" in {
    val boundInputs = inputs.copy(knownFree =
      inputs.knownFree.newBindings(List((Some("x"), NameSort)))._1)
    
    an [Error] should be thrownBy {
      NameNormalizeMatcher.normalizeMatch(nvar, boundInputs)
    }
  }

  val nqvar = new NameQuote(new PVar("x"))

  "NameQuote" should "compile to a quoted var if the var is bound" in {
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), ProcSort)))._1)
    val nqvar = new NameQuote(new PVar("x"))
    val result = NameNormalizeMatcher.normalizeMatch(nqvar, boundInputs)
    result.chan should be (Quote(Par().copy(exprs = List(EVar(BoundVar(0))))))
    result.knownFree should be (inputs.knownFree)
  }

  "NameQuote" should "return a free use if the quoted proc has a free var" in {
    val result = NameNormalizeMatcher.normalizeMatch(nqvar, inputs)
    result.chan should be (Quote(Par().copy(exprs = List(EVar(FreeVar(0))))))
    result.knownFree should be (inputs.knownFree.newBindings(List((Some("x"), ProcSort)))._1)
  }

  "NameQuote" should "compile to a quoted ground" in {
    val nqground = new NameQuote(new PGround(new GroundInt(7)))
    val result = NameNormalizeMatcher.normalizeMatch(nqground, inputs)
    result.chan should be (Quote(Par().copy(exprs = List(GInt(7)))))
    result.knownFree should be (inputs.knownFree)
  }

  "NameQuote" should "collapse an eval" in {
    val nqeval = new NameQuote(new PEval(new NameVar("x")))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)
    val result = NameNormalizeMatcher.normalizeMatch(nqeval, boundInputs)
    result.chan should be (ChanVar(BoundVar(0)))
    result.knownFree should be (inputs.knownFree)
  }

  "NameQuote" should "not collapse an eval | eval" in {
    val nqeval = new NameQuote(new PPar(new PEval(new NameVar("x")), new PEval(new NameVar("x"))))
    val boundInputs = inputs.copy(env =
      inputs.env.newBindings(List((Some("x"), NameSort)))._1)
    val result = NameNormalizeMatcher.normalizeMatch(nqeval, boundInputs)
    result.chan should be (Quote(Par().copy(evals = List(Eval(ChanVar(BoundVar(0))), Eval(ChanVar(BoundVar(0)))))))
    result.knownFree should be (inputs.knownFree)
  }
}
