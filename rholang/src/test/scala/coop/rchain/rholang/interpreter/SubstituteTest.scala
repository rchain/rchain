package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance.{EEvalBody, EVarBody, GString}
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.{GPrivate => _, _}
import coop.rchain.rholang.interpreter.Substitute._
import coop.rchain.rholang.interpreter.errors.SubstituteError
import coop.rchain.rholang.interpreter.implicits._
import monix.eval.Coeval
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class VarSubSpec extends FlatSpec with Matchers {

  "FreeVar" should "throw an error" in {
    val source: Par            = GPrivate()
    implicit val env: Env[Par] = Env.makeEnv(source)
    an[SubstituteError] should be thrownBy maybeSubstitute[Coeval](FreeVar(0)).value
  }

  "BoundVar" should "be substituted for process" in {
    val source: Par            = GPrivate()
    implicit val env: Env[Par] = Env.makeEnv(source)
    val result                 = maybeSubstitute[Coeval](BoundVar(0)).value
    result should be(Right(source))
  }

  "BoundVar" should "be substituted with expression" in {
    val source: Par            = Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0))
    implicit val env: Env[Par] = Env.makeEnv(source)
    val result                 = maybeSubstitute[Coeval](BoundVar(0)).value
    result should be(Right(source))
  }

  "BoundVar" should "be left unchanged" in {
    implicit val env        = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result              = maybeSubstitute[Coeval](BoundVar(0)).value
    val expectedResult: Var = BoundVar(0)
    result should be(Left(expectedResult))
  }
}

class ChannelSubSpec extends FlatSpec with Matchers {
  "ChanVar" should "be left unchanged" in {
    implicit val env            = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result                  = substituteChannel[Coeval].substitute(ChanVar(BoundVar(0))).value
    val expectedResult: Channel = ChanVar(BoundVar(0))
    result should be(expectedResult)
  }

  "Quote" should "leave variables not in environment alone." in {
    implicit val env = Env.makeEnv(GPrivate(): Par).shift(1)
    val par          = Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0))
    val target       = Quote(par)
    val result       = substituteChannel[Coeval].substitute(target).value
    val expectedResult: Channel =
      Quote(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0)))
    result should be(expectedResult)
  }

  "Channel" should "be substituted for a Quote" in {
    val source: Par             = GPrivate()
    implicit val env            = Env.makeEnv(source)
    val result                  = substituteChannel[Coeval].substitute(ChanVar(BoundVar(0))).value
    val expectedResult: Channel = Quote(source)
    result should be(expectedResult)
  }
}

class SendSubSpec extends FlatSpec with Matchers {
  "Send" should "leave variables not in evironment alone." in {

    implicit val env = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result =
      substituteSend[Coeval]
        .substitute(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0)))
        .value
    result should be(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0)))
  }

  "Send" should "substitute Channel for Quote" in {
    val source0: Par = GPrivate()
    val source1: Par = GPrivate()
    implicit val env = Env.makeEnv(source0, source1)
    val result = substituteSend[Coeval]
      .substitute(
        Send(Quote(Send(ChanVar(BoundVar(1)), List(Par()), false, BitSet(1))),
             List(Par()),
             false,
             BitSet(1)))
      .value
    result should be(
      Send(
        Quote(Send(Quote(source0), List(Par()), false, BitSet())),
        List(Par()),
        false,
        BitSet()
      )
    )
  }

  "Send" should "substitute all Channels for Quotes" in {
    val source: Par  = GPrivate()
    implicit val env = Env.makeEnv(source)
    val target = Send(ChanVar(BoundVar(0)),
                      List(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0))),
                      false,
                      BitSet(0))
    val _target =
      Send(Quote(source), List(Send(Quote(source), List(Par()), false, BitSet())), false, BitSet())
    val result = substituteSend[Coeval].substitute(target).value
    result should be(_target)
  }

  "Send" should "substitute all channels for quoted process in environment" in {
    val chan0        = ChanVar(BoundVar(0))
    val source: Par  = New(1, Send(chan0, List(Par()), false, BitSet(0)), BitSet())
    implicit val env = Env.makeEnv(source)
    val target =
      Send(chan0, List(Send(chan0, List(Par()), false, BitSet(0))), false, BitSet(0))
    val result = substituteSend[Coeval].substitute(target).value
    result should be(
      Send(
        Quote(New(1, Send(chan0, List(Par()), false, BitSet(0)), BitSet())),
        List(
          Send(
            Quote(New(1, Send(chan0, List(Par()), false, BitSet(0)), BitSet())),
            List(Par()),
            false,
            BitSet()
          )
        ),
        false,
        BitSet()
      )
    )
  }
}

class NewSubSpec extends FlatSpec with Matchers {
  "New" should "only substitute body of expression" in {
    val source: Par  = GPrivate()
    implicit val env = Env.makeEnv(source)
    val target       = New(1, Send(ChanVar(BoundVar(1)), List(Par()), false, BitSet(1)), BitSet(0))
    val result       = substituteNew[Coeval].substitute(target).value
    result should be(New(1, Send(Quote(source), List(Par()), false, BitSet()), BitSet()))
  }

  "New" should "only substitute all Channels in body of express" in {
    val source0: Par           = GPrivate()
    val source1: Par           = GPrivate()
    implicit val env: Env[Par] = Env.makeEnv(source0, source1)
    val target = New(2,
                     Send(ChanVar(BoundVar(3)),
                          List(Send(ChanVar(BoundVar(2)), List(Par()), false, BitSet(2))),
                          false,
                          BitSet(2, 3)),
                     BitSet(0, 1))

    val result = substituteNew[Coeval].substitute(target).value
    result should be(
      New(
        2,
        Send(Quote(source0),
             List(Send(Quote(source1), List(Par()), false, BitSet())),
             false,
             BitSet()),
        BitSet()
      ))
  }
}

class EvalSubSpec extends FlatSpec with Matchers {
  "Eval" should "remove Eval/Quote pairs." in {
    implicit val env: Env[Par] = Env.makeEnv(GPrivate("one"), GPrivate("zero"))
    val target: Par            = EEvalBody(ChanVar(BoundVar(1)))
    val result                 = substitutePar[Coeval].substitute(target).value
    val expected: Par          = GPrivate("one")

    result should be(expected)
  }
}

class BundleSubSpec extends FlatSpec with Matchers {
  "Bundle" should "substitute within the body of the bundle." in {
    val source: Par  = GPrivate()
    implicit val env = Env.makeEnv(source)
    val target       = Bundle(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0)))
    val result       = substituteBundle[Coeval].substitute(target).value
    result should be(
      Bundle(Send(Quote(source), List(Par()), false, BitSet()))
    )
  }

  it should "only substitute all Channels inside body" in {
    val source0: Par           = GPrivate()
    val source1: Par           = GPrivate()
    implicit val env: Env[Par] = Env.makeEnv(source0, source1)
    val target = Bundle(
      Send(ChanVar(BoundVar(1)),
           List(Send(ChanVar(BoundVar(0)), List(Par()), false, BitSet(0))),
           false,
           BitSet(0, 1))
    )
    val result = substituteBundle[Coeval].substitute(target).value
    result should be(
      Bundle(
        Send(Quote(source0),
             List(Send(Quote(source1), List(Par()), false, BitSet())),
             false,
             BitSet())
      ))
  }

  it should "preserve bundles' polarities during substitution" in {
    val r: Par              = Bundle(body = Expr(GString("stdout")), writeFlag = true, readFlag = false) // bundle+ { "stdout" }
    implicit val env        = Env.makeEnv(r)
    val bundle: Par         = Bundle(Expr(EVarBody(EVar(BoundVar(0)))), writeFlag = false, readFlag = true)
    val result              = substitutePar[Coeval].substitute(bundle).value
    val expectedResult: Par = Bundle(Expr(GString("stdout")), writeFlag = false, readFlag = false)

    result should be(expectedResult)
  }
}
