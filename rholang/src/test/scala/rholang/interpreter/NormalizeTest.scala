import coop.rchain.interpreter._
import coop.rchain.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}
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

class NilVisitSpec extends FlatSpec with Matchers {
  val visitor = new ProcNormalizeVisitor() {}
  val inputs = ProcVisitInputs(
      Par(),
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())

  "PNil" should "Compile as no modification to the par object" in {
    val nil = new PNil()

    val result = nil.accept(visitor, inputs)
    result.par should be (inputs.par)
    result.knownFree should be (inputs.knownFree)
  }
}

class ProcVarVisitSpec extends FlatSpec with Matchers {
  val visitor = new ProcNormalizeVisitor() {}
  val inputs = ProcVisitInputs(
      Par(),
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())
  val pvar = new PVar("x")

  "PVar" should "Compile as BoundVar if it's in env" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", ProcSort)))._1)
  
    val result = pvar.accept(visitor, boundinputs)
    result.par should be (inputs.par.copy(expr = List(EVar(BoundVar(0)))))
    result.knownFree should be (inputs.knownFree)
  }
  "PVar" should "Compile as FreeVar if it's not in env" in {
    val result = pvar.accept(visitor, inputs)
    result.par should be (inputs.par.copy(expr = List(EVar(FreeVar(0)))))
    result.knownFree shouldEqual
        (inputs.knownFree.newBindings(
            List(("x", ProcSort)))._1)
  }
  "PVar" should "Not compile if it's in env of the wrong sort" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", NameSort)))._1)
    
    an [Error] should be thrownBy {
      pvar.accept(visitor, boundinputs)
    }
  }
  "PVar" should "Not compile if it's used free somewhere else" in {
    val boundinputs = inputs.copy(knownFree =
      inputs.knownFree.newBindings(List(("x", ProcSort)))._1)
    
    an [Error] should be thrownBy {
      pvar.accept(visitor, boundinputs)
    }
  }
}

class ParVisitSpec extends FlatSpec with Matchers {
  val visitor = new ProcNormalizeVisitor() {}
  val inputs = ProcVisitInputs(
      Par(),
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())

  "PPar" should "Compile both branches into a par object" in {
    val parGround = new PPar(
        new PGround(
            new GroundInt(7)),
        new PGround(
            new GroundInt(8)))
    val result = parGround.accept(visitor, inputs)
    result.par should be (
        inputs.par.copy(expr =
            List(GInt(8), GInt(7))))
    result.knownFree should be (inputs.knownFree)
  }
  "PPar" should "Compile both branches with the same environment" in {
    val parDoubleBound = new PPar(
        new PVar("x"),
        new PVar("x"))
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", ProcSort)))._1)

    val result = parDoubleBound.accept(visitor, boundinputs)
    result.par should be (
        inputs.par.copy(expr =
            List(EVar(BoundVar(0)), EVar(BoundVar(0)))))
    result.knownFree should be (inputs.knownFree)
  }
  "PPar" should "Not compile if both branches use the same free variable" in {
    val parDoubleFree = new PPar(
        new PVar("x"),
        new PVar("x"))
    an [Error] should be thrownBy {
      parDoubleFree.accept(visitor, inputs)
    }
  }
}

class NameWildcardVisitSpec extends FlatSpec with Matchers {
  val procVisitor = new ProcNormalizeVisitor() {}
  val nameVisitor = new NameNormalizeVisitor() {
    val procVisitor = NameWildcardVisitSpec.this.procVisitor
  }
  val inputs = NameVisitInputs(
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())

  "NameWildcard" should "Set wildcard flag in knownFree" in {
    val nw = new NameWildcard()
    val result = nw.accept(nameVisitor, inputs)
    result.chan should be (ChanVar(WildCard()))
    result.knownFree.wildcardUsed should be (true)
    result.knownFree shouldEqual (inputs.knownFree.setWildcardUsed())
  }
}

class NameVarVisitSpec extends FlatSpec with Matchers {
  val procVisitor = new ProcNormalizeVisitor() {}
  val nameVisitor = new NameNormalizeVisitor() {
    val procVisitor = NameVarVisitSpec.this.procVisitor
  }
  val inputs = NameVisitInputs(
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())
  val nvar = new NameVar("x")

  "NameVar" should "Compile as BoundVar if it's in env" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", NameSort)))._1)
  
    val result = nvar.accept(nameVisitor, boundinputs)
    result.chan should be (ChanVar(BoundVar(0)))
    result.knownFree should be (inputs.knownFree)
  }
  "NameVar" should "Compile as FreeVar if it's not in env" in {
    val result = nvar.accept(nameVisitor, inputs)
    result.chan should be (ChanVar(FreeVar(0)))
    result.knownFree shouldEqual
        (inputs.knownFree.newBindings(
            List(("x", NameSort)))._1)
  }
  "NameVar" should "Not compile if it's in env of the wrong sort" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", ProcSort)))._1)
    
    an [Error] should be thrownBy {
      nvar.accept(nameVisitor, boundinputs)
    }
  }
  "NameVar" should "Not compile if it's used free somewhere else" in {
    val boundinputs = inputs.copy(knownFree =
      inputs.knownFree.newBindings(List(("x", NameSort)))._1)
    
    an [Error] should be thrownBy {
      nvar.accept(nameVisitor, boundinputs)
    }
  }
}

class NameQuoteVisitSpec extends FlatSpec with Matchers {
  val procVisitor = new ProcNormalizeVisitor() {}
  val nameVisitor = new NameNormalizeVisitor() {
    val procVisitor = NameQuoteVisitSpec.this.procVisitor
  }
  val inputs = NameVisitInputs(
      DebruijnLevelMap[VarSort](),
      DebruijnLevelMap[VarSort]())
  val nvar = new NameVar("x")

  "NameQuote" should "compile to a quoted var if the var is bound" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", ProcSort)))._1)
    val nqvar = new NameQuote(new PVar("x"))
    val result = nqvar.accept(nameVisitor, boundinputs)
    result.chan should be (Quote(Par().copy(expr = List(EVar(BoundVar(0))))))
    result.knownFree should be (inputs.knownFree)
  }

  "NameQuote" should "return a free use if the quoted proc has a free var" in {
    val nqvar = new NameQuote(new PVar("x"))
    val result = nqvar.accept(nameVisitor, inputs)
    result.chan should be (Quote(Par().copy(expr = List(EVar(FreeVar(0))))))
    result.knownFree should be (inputs.knownFree.newBindings(List(("x", ProcSort)))._1)
  }

  "NameQuote" should "compile to a quoted ground" in {
    val nqground = new NameQuote(new PGround(new GroundInt(7)))
    val result = nqground.accept(nameVisitor, inputs)
    result.chan should be (Quote(Par().copy(expr = List(GInt(7)))))
    result.knownFree should be (inputs.knownFree)
  }
}

