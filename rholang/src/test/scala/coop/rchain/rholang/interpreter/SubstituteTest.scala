package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import Substitute._
import ExprTest._
import scala.collection.immutable.HashMap

object ExprTest {
  val chan0: Channel = ChanVar(BoundVar(0))
  val name0: Quote = Quote(Par.forge)
  val chan1: Channel = ChanVar(BoundVar(1))
  val name1: Quote = Quote(Par.forge)
  val chan2: Channel = ChanVar(BoundVar(2))
  val name2: Quote = Quote(Par.forge)
  val chan3: Channel = ChanVar(BoundVar(3))
  val name3: Quote = Quote(Par.forge)
  val send0: Send = Send(chan0, List(Par()),false,0)
  val send1: Send = Send(chan1, List(Par()),false,0)
  val send2: Send = Send(chan2, List(Par()),false,0)
  val sendQ: Send = Send(Quote(send0),List(Par()),false,0)
}

class ChannelSub extends FlatSpec with Matchers {

  "FreeVar" should "throw an error when substituted" in {
    val env = HashMap(0 -> Par.forge)
    val result = subOrDec(FreeVar(0),env)
    result should be (Error)
   }

  "BoundVar" should "be substituted if environment contains a binding" in {
    val source = Par.forge
    val env = HashMap(0 -> source)
    val result = subOrDec(BoundVar(0),env)
    result should be (Right(source))
  }

  "BoundVar" should "throw an error if environment contains no binding and environment level is greater than variable level" in {
    val env = HashMap(1 -> Par.forge)
    val result = subOrDec(BoundVar(0),env)
    result should be (Error)
  }

  "BoundVar" should "be decremented by environment level if environment contains no binding" in {
    val env = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = subOrDec(BoundVar(2),env)
    result should be (Left(BoundVar(0)))
  }

  "ChanVar" should "be decremented by environment level if environment contains no binding" in {
    val env = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = substitute(chan2,env)
    result should be (chan0)
  }

  "Quote" should "substitute quoted process" in {
    val env = HashMap(0 -> Par.forge)
    val par = Par(send1)
    val target = Quote(par)
    val result = substitute(target, env)
    result should be (send0)
  }

  "Channel" should "throw an error if environment contains binding" in {
    val env = HashMap(0 -> Par.forge)
    val result = substitute(chan0, env)
    result should be (Error)
  }
}

class ParSub extends FlatSpec with Matchers {

  "Send" should "decrement subject channel and substitute object processes" in {
    val env = HashMap(0 -> Par.forge, 1 -> Par.forge)
    val result = substitute(send2,env)
    result should be (send0)
  }

  "Send" should "decrement subject ChanVar" in {
    val source0 = Par.forge
    val source1 = Par.forge
    val env = HashMap(0 -> source0, 1 -> source1)
    val result = substitute(sendQ,env)
    result should be (
      Send(
        Quote(source0),
        List(Par()),
        false,
        0
      )
    )
  }
}
