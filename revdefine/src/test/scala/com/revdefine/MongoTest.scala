package com.revdefine

import cats.effect.Async
import cats.implicits.catsSyntaxApplicativeId
import com.revdefine.node.store.MongoStore
import com.revdefine.node.web.account.Account
import com.revdefine.node.web.transfer.Transfer._
import org.scalatest.{FlatSpec, Matchers}

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.mongodb.scala._
import fs2._
import monix.eval.Task
import org.mongodb.scala.model.UpdateOptions
import org.mongodb.scala.model.Updates.inc
import com.revdefine.syntax.all._
import monix.execution.Scheduler.Implicits.global
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Updates._

import scala.io.Source
class MongoTest extends FlatSpec {
  it should "asd" in {

    val mongoStore = MongoStore[Task]("mongodb://localhost:27017")
    (for {
      r <- mongoStore.transactionCollection
            .updateOne(
              equal("address", "a"),
              inc("balance", 10),
              UpdateOptions().upsert(true)
            )
            .liftToF[Task]
      isUpsertF = Task.delay(r.getUpsertedId.isObjectId)
      isUpsert  <- isUpsertF.redeem(_ => false, _ => true)
      a         = r.getModifiedCount != 1
      _         = println(r)
      _         = println(a)
      _         = println(isUpsert)

    } yield ()).runSyncUnsafe()
  }

  it should "get total on chain rev" in {
    val w = Source
      .fromResource("wallets-908400-hf1.txt")
      .getLines()
      .foldLeft(0L) {
        case (c, s) =>
          s.split(",") match {
            case Array(_, amount) =>
              c + amount.toLong
            case _ => throw new Exception("Invalid wallets file")
          }
      }

    val b = Source
      .fromResource("bonds-hf1.txt")
      .getLines()
      .foldLeft(0L) {
        case (c, s) =>
          s.split(" ") match {
            case Array(_, amount) =>
              c + amount.toLong
            case _ => throw new Exception("Invalid wallets file")
          }
      }
    println(b + w)
  }

}
