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
    val resultTask    = Reduce.debruijnInterpreter.evalExpr(addExpr)(Env())

    val result         = Await.result(resultTask.runAsync, 3.seconds)
    val expected: Expr = GInt(15)
    result should be(expected)
  }

  "evalExpr" should "leave ground values alone" in {
    val groundExpr: Expr = GInt(7)
    val resultTask       = Reduce.debruijnInterpreter.evalExpr(groundExpr)(Env())

    val result         = Await.result(resultTask.runAsync, 3.seconds)
    val expected: Expr = GInt(7)
    result should be(expected)
  }

  "eval of Send" should "place something in the tuplespace." in {
    val send: Send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val resultTask = Reduce.debruijnInterpreter.eval(send)(Env())
    val inspectTask = for {
      _      <- resultTask
      tsHash <- Reduce.debruijnInterpreter.tupleSpace.spaceMVar.take
      _      <- Reduce.debruijnInterpreter.tupleSpace.spaceMVar.put(tsHash)
    } yield tsHash

    val result = Await.result(inspectTask.runAsync, 3.seconds)
    result should be(
      HashMap(
        Quote(GString("channel")) ->
          ((Seq[(Seq[Par], Boolean)]((Seq[Par](GInt(7), GInt(8), GInt(9)), false)),
            Seq[(Seq[Channel], Cont[Par, Par], Boolean)]()))))
  }
}
