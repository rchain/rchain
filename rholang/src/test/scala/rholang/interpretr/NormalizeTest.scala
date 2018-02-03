import coop.rchain.interpreter._
import coop.rchain.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}
import org.scalatest._

class BoolVisitorSpec extends FlatSpec with Matchers {
  val visitor = new BoolNormalizeVisitor() {}
  "BoolTrue" should "Compile as GBool(true)" in {
    val btrue = new BoolTrue()

    btrue.accept(visitor, this) should be (GBool(true))
  }
  "BoolFalse" should "Compile as GBool(false)" in {
    val bfalse = new BoolFalse()

    bfalse.accept(visitor, this) should be (GBool(false))
  }
}

class GroundVisitorSpec extends FlatSpec with Matchers {
  val visitor = new GroundNormalizeVisitor() {}
  "GroundInt" should "Compile as GInt" in {
    val gi = new GroundInt(7)

    gi.accept(visitor, this) should be (GInt(7))
  }
  "GroundString" should "Compile as GString" in {
    val gs = new GroundString("String")

    gs.accept(visitor, this) should be (GString("String"))
  }
  "GroundUri" should "Compile as GUri" in {
    val gu = new GroundUri("Uri")

    gu.accept(visitor, this) should be (GUri("Uri"))
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

class VarVisitSpec extends FlatSpec with Matchers {
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
    result.knownFree should be
        (inputs.knownFree.newBindings(
            List(("x", ProcSort)))._1)
  }
  "PVar" should "Not compile if it's in env of the wrong sort" in {
    val boundinputs = inputs.copy(env =
      inputs.env.newBindings(List(("x", NameSort)))._1)
    
    an [Error] should be thrownBy {
      val result = pvar.accept(visitor, boundinputs)
    }
  }
  "PVar" should "Not compile if it's used free somewhere else" in {
    val boundinputs = inputs.copy(knownFree =
      inputs.knownFree.newBindings(List(("x", ProcSort)))._1)
    
    an [Error] should be thrownBy {
      val result = pvar.accept(visitor, boundinputs)
    }
  }
}
