package coop.rchain.rholang.interpreter.accounting
import cats.effect.concurrent.Ref
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{Assertion, FlatSpec}

import scala.concurrent.Await
import scala.concurrent.duration._

class CostAccountingSpec extends FlatSpec with TripleEqualsSupport {

  behavior of "CostAccountingAlg"

  val defaultCost = CostAccount(3, Cost(33))

  def withCostAccAlg[T](
      initState: CostAccount = defaultCost
  )(f: CostAccounting[Task] => Task[T]): T = {
    val test = for {
      alg <- CostAccounting.of[Task](initState)
      res <- f(alg)
    } yield res
    Await.result(test.runToFuture, 5.seconds)
  }

  "get" should "return current cost account" in withCostAccAlg() { alg =>
    for {
      s <- alg.get()
    } yield assert(s === defaultCost)
  }

  "charge" should "modify underlying cost account" in withCostAccAlg() { alg =>
    val c = Cost(13)
    for {
      _ <- alg.charge(c)
      s <- alg.get()
    } yield assert(defaultCost - c === s)
  }

  def assertOutOfPhloError[A](test: Task[A]): Task[Assertion] =
    test.attempt.map(res => assert(res === Left(OutOfPhlogistonsError)))

  it should "fail when cost account goes below zero" in withCostAccAlg() { alg =>
    val cost = Cost(100)
    val test = for {
      _ <- alg.charge(cost)
      s <- alg.get()
    } yield s

    assertOutOfPhloError(test)
  }

  it should "fail when tries to charge on the cost account that is already negative" in withCostAccAlg(
    CostAccount(-100)
  ) { alg =>
    val cost = Cost(1)
    val test = for {
      _ <- alg.charge(cost)
      s <- alg.get()
    } yield s

    assertOutOfPhloError(test)
  }
}
