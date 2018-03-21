package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import Substitute._
import Env._
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.{GPrivate => _, _}
import implicits._

import scala.collection.immutable.BitSet

class VarSubSpec extends FlatSpec with Matchers {
  "FreeVar" should "throw an error" in {
    val source: Par   = GPrivate()
    val env: Env[Par] = Env.makeEnv(source)
    an[Error] should be thrownBy maybeSubstitute(FreeVar(0))(env)
  }

  "BoundVar" should "be substituted for process" in {
    val source: Par   = GPrivate()
    val env: Env[Par] = Env.makeEnv(source)
    val result        = maybeSubstitute(BoundVar(0))(env)
    result should be(Right(source))
  }

  "BoundVar" should "be substituted with expression" in {
    val source: Par   = Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0))
    val env: Env[Par] = Env.makeEnv(source)
    val result        = maybeSubstitute(BoundVar(0))(env)
    result should be(Right(source))
  }

  "BoundVar" should "be left unchanged" in {
    val env                 = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result              = maybeSubstitute(BoundVar(0))(env)
    val expectedResult: Var = BoundVar(0)
    result should be(Left(expectedResult))
  }
}

class ChannelSubSpec extends FlatSpec with Matchers {
  "ChanVar" should "be left unchanged" in {
    val env                     = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result                  = substitute(ChanVar(BoundVar(0)))(env)
    val expectedResult: Channel = ChanVar(BoundVar(0))
    result should be(expectedResult)
  }

  "Quote" should "leave variables not in environment alone." in {
    val env             = Env.makeEnv(GPrivate(): Par).shift(1)
    val par             = Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0))
    val target          = Quote(par)
    val result: Channel = substitute(target)(env)
    val expectedResult: Channel =
      Quote(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)))
    result should be(expectedResult)
  }

  "Channel" should "be substituted for a Quote" in {
    val source: Par             = GPrivate()
    val env                     = Env.makeEnv(source)
    val result                  = substitute(ChanVar(BoundVar(0)))(env)
    val expectedResult: Channel = Quote(source)
    result should be(expectedResult)
  }
}

class SendSubSpec extends FlatSpec with Matchers {
  "Send" should "leave variables not in evironment alone." in {

    val env    = Env.makeEnv(GPrivate(): Par, GPrivate(): Par).shift(1)
    val result = substitute(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)))(env)
    result should be(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)))
  }

  "Send" should "substitute Channel for Quote" in {
    val source0: Par = GPrivate()
    val source1: Par = GPrivate()
    val env          = Env.makeEnv(source0, source1)
    val result = substitute(
      Send(Quote(Send(ChanVar(BoundVar(1)), List(Par()), false, 0, BitSet(1))),
           List(Par()),
           false,
           0,
           BitSet(1)))(env)
    result should be(
      Send(
        Quote(Send(Quote(source0), List(Par()), false, 0, BitSet())),
        List(Par()),
        false,
        0,
        BitSet()
      )
    )
  }

  "Send" should "substitute all Channels for Quotes" in {
    val source: Par = GPrivate()
    val env         = Env.makeEnv(source)
    val target = Send(ChanVar(BoundVar(0)),
                      List(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0))),
                      false,
                      0,
                      BitSet(0))
    val _target =
      Send(Quote(source),
           List(Send(Quote(source), List(Par()), false, 0, BitSet())),
           false,
           0,
           BitSet())
    val result = substitute(target)(env)
    result should be(_target)
  }

  "Send" should "substitute all channels for quoted process in environment" in {
    val chan0       = ChanVar(BoundVar(0))
    val source: Par = New(1, Send(chan0, List(Par()), false, 0, BitSet(0)), BitSet())
    val env         = Env.makeEnv(source)
    val target =
      Send(chan0, List(Send(chan0, List(Par()), false, 0, BitSet(0))), false, 0, BitSet(0))
    val result = substitute(target)(env)
    result should be(
      Send(
        Quote(New(1, Send(chan0, List(Par()), false, 0, BitSet(0)), BitSet())),
        List(
          Send(
            Quote(New(1, Send(chan0, List(Par()), false, 0, BitSet(0)), BitSet())),
            List(Par()),
            false,
            0,
            BitSet()
          )
        ),
        false,
        0,
        BitSet()
      )
    )
  }
}

class NewSubSpec extends FlatSpec with Matchers {
  "New" should "only substitute body of expression" in {
    val source: Par = GPrivate()
    val env         = Env.makeEnv(source)
    val target      = New(1, Send(ChanVar(BoundVar(1)), List(Par()), false, 0, BitSet(1)), BitSet(0))
    val result      = substitute(target)(env)
    result should be(
      New(1, Send(Quote(source), List(Par()), false, 0, BitSet()), BitSet())
    )
  }

  "New" should "only substitute all Channels in body of express" in {
    val source0: Par  = GPrivate()
    val source1: Par  = GPrivate()
    val env: Env[Par] = Env.makeEnv(source0, source1)
    val target = New(2,
                     Send(ChanVar(BoundVar(3)),
                          List(Send(ChanVar(BoundVar(2)), List(Par()), false, 0, BitSet(2))),
                          false,
                          0,
                          BitSet(2, 3)),
                     BitSet(0, 1))

    val result = substitute(target)(env)
    result should be(
      New(
        2,
        Send(Quote(source0),
             List(Send(Quote(source1), List(Par()), false, 0, BitSet())),
             false,
             0,
             BitSet()),
        BitSet()
      )
    )
  }
}
