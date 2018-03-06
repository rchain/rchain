package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import Substitute._
import scala.collection.immutable.HashMap
import SubstituteTest._

object SubstituteTest {
  val chan0: Channel = ChanVar(BoundVar(0))
  val name0: Quote   = Quote(Par.forge)
  val chan1: Channel = ChanVar(BoundVar(1))
  val name1: Quote   = Quote(Par.forge)
  val chan2: Channel = ChanVar(BoundVar(2))
  val name2: Quote   = Quote(Par.forge)
  val send0: Send    = Send(chan0, List(Par()), false, 0)
  val send1: Send    = Send(chan1, List(Par()), false, 0)
  val send2: Send    = Send(chan2, List(Par()), false, 0)
  val sendQ: Send    = Send(Quote(send0), List(Par()), false, 0)
}

class VarSubSpec extends FlatSpec with Matchers {

  "FreeVar" should "throw an error" in {
    val env = HashMap(0 -> Par.forge)
    an[Error] should be thrownBy subOrDec(FreeVar(0))(env)
  }

  "BoundVar" should "be substituted for process" in {
    val source = Par.forge
    val env    = HashMap(0 -> source)
    val result = subOrDec(BoundVar(0))(env)
    result should be(Right(source))
  }

  "BoundVar" should "be substituted with renamed expression" in {
    val source  = Par(send0)
    val _source = Par(send1)
    val env     = HashMap(0 -> source)
    val result  = subOrDec(BoundVar(0))(env)
    result should be(Right(_source))
  }

  "BoundVar" should "throw an error" in {
    val env = HashMap(1 -> Par.forge)
    an[Error] should be thrownBy subOrDec(BoundVar(0))(env)
  }

  "BoundVar" should "be decremented by environment level" in {
    val env    = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = subOrDec(BoundVar(2))(env)
    result should be(Left(BoundVar(0)))
  }
}

class ChannelSubSpec extends FlatSpec with Matchers {

  "ChanVar" should "be decremented by environment level" in {
    val env    = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = substitute(chan2)(env)
    result should be(chan0)
  }

  "Quote" should "substitute quoted process" in {
    val env    = HashMap(0 -> Par.forge)
    val par    = Par(send1)
    val target = Quote(par)
    val result = substitute(target)(env)
    result should be(Quote(Par(send0)))
  }

  "Channel" should "be substituted for a Quote" in {
    val source = Par.forge
    val env    = HashMap(0 -> source)
    val result = substitute(chan0)(env)
    result should be(Quote(source))
  }
}

class SendSubSpec extends FlatSpec with Matchers {

  "Send" should "decrement subject Channel and substitute object processes" in {
    val env    = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = substitute(send2)(env)
    result should be(send0)
  }

  "Send" should "substitute Channel for Quote" in {
    val source0 = Par.forge
    val source1 = Par.forge
    val env     = HashMap(0 -> source0, 1 -> source1)
    val result  = substitute(sendQ)(env)
    result should be(
      Send(
        Quote(Send(Quote(source0), List(Par()), false, 0)),
        List(Par()),
        false,
        0
      )
    )
  }

  "Send" should "substitute all Channels for Quotes" in {
    val source = Par.forge
    val target = Send(chan0, List(Par(send0)), false, 0)
    val _target =
      Send(Quote(source), List(Par(Send(Quote(source), List(Par()), false, 0))), false, 0)
    val env    = HashMap(0 -> source)
    val result = substitute(target)(env)
    result should be(_target)
  }

  "Send" should "substitute all channels for renamed, quoted process in environment" in {
    val source = Par(send0)
    val target = Send(chan0, List(Par(send0)), false, 0)
    val env    = HashMap(0 -> source)
    val result = substitute(target)(env)
    result should be(
      Send(
        Quote(send1),
        List(
          Par(
            Send(
              Quote(Par(send1)),
              List(Par()),
              false,
              0
            )
          )
        ),
        false,
        0
      )
    )
  }
}

class NewSubSpec extends FlatSpec with Matchers {

  "New" should "only substitute body of expression" in {
    val source = Par.forge
    val env    = HashMap(0 -> source)
    val target = New(1, Par(Send(chan0, List(Par()), false, 0)))
    val result = substitute(target)(env)
    result should be(
      New(1, Par(Send(Quote(source), List(Par()), false, 0)))
    )
  }

  "New" should "only substitute all Channels in body of express" in {
    val source0 = Par.forge
    val source1 = Par.forge
    val env     = HashMap(0 -> source0, 1 -> source1)
    val target  = New(2, Par(Send(chan0, List(Par(send1)), false, 0)))
    val result  = substitute(target)(env)
    result should be(
      New(
        2,
        Par(
          Send(Quote(source0), List(Par(Send(Quote(source1), List(Par()), false, 0))), false, 0)
        )
      )
    )
  }
}
