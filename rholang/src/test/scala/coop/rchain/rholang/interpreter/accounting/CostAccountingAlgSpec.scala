package coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.Outcome
import org.scalatest.fixture.FlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class CostAccountingAlgSpec extends FlatSpec with TripleEqualsSupport {

  behavior of "CostAccountingAlg"

  val defaultCost = CostAccount(3, Cost(33))

  "set" should "set the cost account" in { alg =>
    val newState = CostAccount(10, Cost(100L))
    val test = for {
      _ <- alg.set(newState)
      n <- alg.get()
    } yield assert(n === newState)

    Await.ready(test.runAsync, 3.seconds)
  }

  "get" should "return current cost account" in { alg =>
    val test = for {
      s <- alg.get()
    } yield assert(s === defaultCost)

    Await.result(test.runAsync, 1.second)
  }

  "charge" should "modify underlying cost account" in { alg =>
    val c = Cost(13)
    val test = for {
      _ <- alg.charge(c)
      s <- alg.get()
    } yield assert(defaultCost.charge(c) === s)

    Await.result(test.runAsync, 1.second)
  }

  it should "fail when cost account goes below zero" in { alg =>
    // this doesn't make sense for now but because we still
    // add up costs rather than charge we have to do it this way
    // TODO: Fix once CostAccountingAlg starts subtracting
    val negativeCost = Cost(-100)
    val test = for {
      _ <- alg.charge(negativeCost)
      s <- alg.get()
    } yield s

    val res = Await.result(test.attempt.runAsync, 1.second)
    assert(res === Left(OutOfPhlogistonsError))
  }

  it should "fail when tries to charge on the cost account that is already negative" in { alg =>
    val cost = Cost(1)
    val negativeAccount = CostAccount(-100)
    val test = for {
      _ <- alg.set(negativeAccount)
      _ <- alg.charge(cost)
      s <- alg.get()
    } yield s

    val res = Await.result(test.attempt.runAsync, 1.second)
    assert(res === Left(OutOfPhlogistonsError))
  }

  override protected def withFixture(
      test: OneArgTest): Outcome = {
    val alg = CostAccountingAlg.unsafe[Task](defaultCost)
    test(alg)
  }
  override type FixtureParam = CostAccountingAlg[Task]
}
