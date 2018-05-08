package coop.rchain.rosette.prim

import coop.rchain.rosette.Location._
import coop.rchain.rosette.prim.ctxt.ctxtRtn
import coop.rchain.rosette.{ArgRegister, Ctxt, Fixnum, Ob, TblObject, Tuple}
import org.scalatest.{FlatSpec, Matchers}

class CtxtSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt.empty
    .copy(id = 0)
    .copy(ctxt = Ctxt.empty.copy(id = 1))

  val globalEnv = TblObject(Seq())

  val initial = (globalEnv, ctxt)

  "ctxt-rtn" should "correctly save the value to a particular location in its continuation and return Right(Ob.NIV)" in {
    val ctxt0 = ctxt.copy(tag = LocTrgt, id = 1)
    val ctxt1 = ctxt.copy(argvec = Tuple(elem = Seq(ctxt0, Fixnum(1))))

    val (_, (_, ctxt2), res) = ctxtRtn.fn.run((), (globalEnv, ctxt1)).value
    ctxt2.arg(0).map {
      case ctxt: Ctxt => ctxt.ctxt.trgt
    } should be(Some(Fixnum(1)))
    res should be(Right(Ob.NIV))
  }

  it should "return Right(Ob.INVALID) when there is error occur in storing a value to the location" in {
    val ctxt0            = ctxt.copy(tag = ArgRegister(10))
    val ctxt1            = ctxt.copy(argvec = Tuple(elem = Seq(ctxt0, Fixnum(1))))
    val (_, (_, _), res) = ctxtRtn.fn.run((), (globalEnv, ctxt1)).value
    res should be(Right(Ob.INVALID))
  }

  it should "return Left(TypeMismatch) when first argument is not a Ctxt" in {
    val ctxt1            = ctxt.copy(argvec = Tuple(elem = Seq(Fixnum(0), Fixnum(1))))
    val (_, (_, _), res) = ctxtRtn.fn.run((), (globalEnv, ctxt1)).value
    res should be(Left(TypeMismatch(0, Ctxt.getClass.getName)))
  }
}
