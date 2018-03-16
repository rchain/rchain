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
import scala.concurrent.Await
import scala.concurrent.duration._

import monix.eval.{MVar, Task}
import monix.execution.Scheduler.Implicits.global

class ReduceSpec extends FlatSpec with Matchers {
  "evalExpr" should "handle simple addition" in {
    val addExpr: Expr = EPlus(GInt(7), GInt(8))
    val resultTask    = Reduce.debruijnInterpreter.evalExpr(addExpr)(Env())

    val result         = Await.result(resultTask.runAsync, 3.seconds)
    val expected: Expr = GInt(15)
    result should be(expected)
  }
}
