package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import Substitute._
import Env._
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.{GPrivate => _, _}
import implicits._

import scala.collection.immutable.BitSet
import scala.collection.mutable.HashMap
import scala.concurrent.Await
import scala.concurrent.duration._

import monix.eval.{MVar, Task}
import monix.execution.Scheduler.Implicits.global

import Reduce.Cont

class ReduceSpec extends FlatSpec with Matchers {
  "evalExpr" should "handle simple addition" in {
    val addExpr: Expr = EPlus(GInt(7), GInt(8))
    val resultTask    = Reduce.makeInterpreter.evalExpr(addExpr)(Env())

    val result         = Await.result(resultTask.runAsync, 3.seconds)
    val expected: Expr = GInt(15)
    result should be(expected)
  }

  "evalExpr" should "leave ground values alone" in {
    val groundExpr: Expr = GInt(7)
    val resultTask       = Reduce.makeInterpreter.evalExpr(groundExpr)(Env())

    val result         = Await.result(resultTask.runAsync, 3.seconds)
    val expected: Expr = GInt(7)
    result should be(expected)
  }

  "eval of Send" should "place something in the tuplespace." in {
    val send: Send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val interpreter = Reduce.makeInterpreter
    val resultTask  = interpreter.eval(send)(Env())
    val inspectTask = for {
      _      <- resultTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(tsHash)
    } yield tsHash

    val result = Await.result(inspectTask.runAsync, 3.seconds)
    result should be(
      HashMap(
        Quote(GString("channel")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GInt(7), GInt(8), GInt(9)), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]()))))
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    val receive: Receive =
      Receive(Seq(
                ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                            Quote(GString("channel")))),
              Par(),
              false,
              3,
              0,
              BitSet())
    val interpreter = Reduce.makeInterpreter
    val resultTask  = interpreter.eval(receive)(Env())
    val inspectTask = for {
      _      <- resultTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(tsHash)
    } yield tsHash

    val result = Await.result(inspectTask.runAsync, 3.seconds)
    result should be(
      HashMap(
        Quote(GString("channel")) ->
          ((Seq[(Seq[Par], Boolean)](),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)](
              (Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
               (Par(), Env()),
               false)
            )))))
  }

  "eval of Send | Receive" should "meet in the tuplespace and proceed." in {
    val send: Send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val receive: Receive =
      Receive(
        Seq(
          ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                      Quote(GString("channel")))),
        Send(Quote(GString("result")), List(GString("Success")), false, 0, BitSet()),
        false,
        3,
        0,
        BitSet()
      )

    val interpreter = Reduce.makeInterpreter
    val sendTask    = interpreter.eval(send)(Env())
    val receiveTask = interpreter.eval(receive)(Env())

    val inspectTaskSendFirst = for {
      _      <- sendTask
      _      <- receiveTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(HashMap())
    } yield tsHash

    val inspectTaskReceiveFirst = for {
      _      <- receiveTask
      _      <- sendTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(HashMap())
    } yield tsHash

    val sendFirstResult = Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    sendFirstResult should be(
      HashMap(
        Quote(GString("result")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GString("Success")), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]())),
        Quote(GString("channel")) -> ((Seq(), Seq()))
      )
    )
    val receiveFirstResult = Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    receiveFirstResult should be(
      HashMap(
        Quote(GString("result")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GString("Success")), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]())),
        Quote(GString("channel")) -> ((Seq(), Seq()))
      )
    )
  }

  "eval of Send on (7 + 8) | Receive on 15" should "meet in the tuplespace and proceed." in {
    val send: Send =
      Send(Quote(EPlus(GInt(7), GInt(8))), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val receive: Receive =
      Receive(
        Seq(
          ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                      Quote(GInt(15)))),
        Send(Quote(GString("result")), List(GString("Success")), false, 0, BitSet()),
        false,
        3,
        0,
        BitSet()
      )

    val interpreter = Reduce.makeInterpreter
    val sendTask    = interpreter.eval(send)(Env())
    val receiveTask = interpreter.eval(receive)(Env())

    val inspectTaskSendFirst = for {
      _      <- sendTask
      _      <- receiveTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(HashMap())
    } yield tsHash

    val inspectTaskReceiveFirst = for {
      _      <- receiveTask
      _      <- sendTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(HashMap())
    } yield tsHash

    val sendFirstResult = Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    sendFirstResult should be(
      HashMap(
        Quote(GString("result")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GString("Success")), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]())),
        Quote(GInt(15)) -> ((Seq(), Seq()))
      )
    )
    val receiveFirstResult = Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    receiveFirstResult should be(
      HashMap(
        Quote(GString("result")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GString("Success")), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]())),
        Quote(GInt(15)) -> ((Seq(), Seq()))
      )
    )
  }

  "Simple match" should "capture and add to the environment." in {
    val pattern: Par =
      Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, 2, BitSet())
    val sendTarget: Par =
      Send(ChanVar(BoundVar(1)), List(GInt(7), EVar(BoundVar(0))), false, 0, BitSet(0, 1))
    val matchTerm: Match =
      Match(sendTarget,
            List(
              MatchCase(
                pattern,
                Send(Quote(GString("result")),
                     List(Eval(ChanVar(BoundVar(1))), EVar(BoundVar(0))),
                     false,
                     0,
                     BitSet())
              )),
            0,
            BitSet(0, 1))
    val env         = Env.makeEnv[Par](GPrivate("one"), GPrivate("zero"))
    val interpreter = Reduce.makeInterpreter
    val matchTask   = interpreter.eval(matchTerm)(env)

    val inspectTask = for {
      _      <- matchTask
      tsHash <- interpreter.tupleSpace.spaceMVar.take
      _      <- interpreter.tupleSpace.spaceMVar.put(tsHash)
    } yield tsHash

    val result = Await.result(inspectTask.runAsync, 3.seconds)
    result should be(
      HashMap(
        Quote(GString("result")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GPrivate("one"), GPrivate("zero")), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]())))
    )
  }
}
