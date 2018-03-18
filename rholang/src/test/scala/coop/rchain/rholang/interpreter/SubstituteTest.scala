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
    val env: Env[Par] = Env(source)
    an[Error] should be thrownBy subOrDec(FreeVar(0))(env)
  }

  "BoundVar" should "be substituted for process" in {
    val source: Par   = GPrivate()
    val env: Env[Par] = Env(source)
    val result        = subOrDec(BoundVar(0))(env)
    result should be(Right(source))
  }

  "BoundVar" should "be substituted with renamed expression" in {
    val source: Par   = Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0))
    val _source: Par  = Send(ChanVar(BoundVar(1)), List(Par()), false, 0, BitSet(1))
    val env: Env[Par] = Env(source)
    val result        = subOrDec(BoundVar(0))(env)
    result should be(Right(_source))
  }

  "BoundVar" should "throw an error" in {
    val env: Env[Par] = Env(1 -> (GPrivate(): Par))
    an[Error] should be thrownBy subOrDec(BoundVar(0))(env)
  }

  "BoundVar" should "be decremented by environment level" in {
    val env                 = Env(GPrivate(): Par, GPrivate(): Par)
    val result              = subOrDec(BoundVar(2))(env)
    val expectedResult: Var = BoundVar(0)
    result should be(Left(expectedResult))
  }
}

class ChannelSubSpec extends FlatSpec with Matchers {

  "ChanVar" should "be decremented by environment level" in {
    val env                     = Env(GPrivate(): Par, GPrivate(): Par)
    val result                  = substitute(ChanVar(BoundVar(2)))(env)
    val expectedResult: Channel = ChanVar(BoundVar(0))
    result should be(expectedResult)
  }

  "Quote" should "substitute quoted process" in {
    val env    = Env(GPrivate(): Par)
    val par    = Send(ChanVar(BoundVar(1)), List(Par()), false, 0, BitSet(1))
    val target = Quote(par)
    val result = substitute(target)(env)
    val expectedResult: Channel =
      Quote(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)))
    result should be(expectedResult)
  }

  "Channel" should "be substituted for a Quote" in {
    val source: Par             = GPrivate()
    val env                     = Env(source)
    val result                  = substitute(ChanVar(BoundVar(0)))(env)
    val expectedResult: Channel = Quote(source)
    result should be(expectedResult)
  }

}

class SendSubSpec extends FlatSpec with Matchers {

  "Send" should "decrement subject Channel and substitute object processes" in {

    val env    = Env(GPrivate(): Par, GPrivate(): Par)
    val result = substitute(Send(ChanVar(BoundVar(2)), List(Par()), false, 0, BitSet(2)))(env)
    result should be(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)))
  }

  "Send" should "substitute Channel for Quote" in {
    val source0: Par = GPrivate()
    val source1: Par = GPrivate()
    val env          = Env(source0, source1)
    val result = substitute(
      Send(Quote(Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0))),
           List(Par()),
           false,
           0,
           BitSet(0)))(env)
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
    val env         = Env(source)
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

  "Send" should "substitute all channels for renamed, quoted process in environment" in {
    val chan0       = ChanVar(BoundVar(0))
    val chan1       = ChanVar(BoundVar(1))
    val source: Par = New(1, Send(chan0, List(Par()), false, 0, BitSet(0)), BitSet())
    val env         = Env(source)
    val target =
      Send(chan0, List(Send(chan0, List(Par()), false, 0, BitSet(0))), false, 0, BitSet(0))
    val result = substitute(target)(env)
    result should be(
      Send(
        Quote(New(1, Send(chan1, List(Par()), false, 0, BitSet(1)), BitSet())),
        List(
          Send(
            Quote(New(1, Send(chan1, List(Par()), false, 0, BitSet(1)), BitSet())),
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
    val env         = Env(source)
    val target      = New(1, Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet(0)), BitSet(0))
    val result      = substitute(target)(env)
    result should be(
      New(1, Send(Quote(source), List(Par()), false, 0, BitSet()), BitSet())
    )
  }

  "New" should "only substitute all Channels in body of express" in {
    val source0: Par  = GPrivate()
    val source1: Par  = GPrivate()
    val env: Env[Par] = Env(source0, source1)
    val target = New(2,
                     Send(ChanVar(BoundVar(0)),
                          List(Send(ChanVar(BoundVar(1)), List(Par()), false, 0, BitSet(1))),
                          false,
                          0),
                     BitSet(0, 1))

    val result = substitute(target)(env)
    result should be(
      New(
        2,
        Send(Quote(source0),
             List(Send(Quote(source1), List(Par()), false, 0, BitSet())),
             false,
             0,
             BitSet())
      )
    )
  }
}
