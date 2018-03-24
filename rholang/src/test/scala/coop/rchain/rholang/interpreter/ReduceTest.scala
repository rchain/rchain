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
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import coop.rchain.storage.{InMemoryStore, Serialize}
import coop.rchain.storage.internal.{Datum, Row, WaitingContinuation}

trait InMemoryStoreTester {
  def withTestStore[C, P, A, K <: Serializable, R](store: InMemoryStore[C, P, A, K])(
      f: InMemoryStore[C, P, A, K] => R): R =
    try {
      f(store)
    } finally {
      store.clear()
    }
}

class ReduceSpec extends FlatSpec with Matchers with InMemoryStoreTester {
  implicit val serializer = Serialize.mkProtobufInstance(Channel)
  val testStore           = InMemoryStore.create[Channel, List[Channel], List[Channel], Par]

  "evalExpr" should "handle simple addition" in {

    val result = withTestStore(testStore) { store =>
      val addExpr    = EPlus(GInt(7), GInt(8))
      val resultTask = Reduce.makeInterpreter(store).evalExpr(addExpr)(Env())
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected: Expr = GInt(15)
    result should be(expected)
  }

  "evalExpr" should "leave ground values alone" in {
    val result = withTestStore(testStore) { store =>
      val groundExpr = GInt(7)
      val resultTask = Reduce.makeInterpreter(store).evalExpr(groundExpr)(Env())
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected: Expr = GInt(7)
    result should be(expected)
  }

  "eval of Send" should "place something in the tuplespace." in {
    val result = withTestStore(testStore) { store =>
      val send =
        Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
      val interpreter = Reduce.makeInterpreter(store)
      val resultTask  = interpreter.eval(send)(Env())
      val inspectTask = for {
        _ <- resultTask
      } yield store.toHashMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("channel")))) ->
          Row(
            Some(
              List(
                Datum[List[Channel]](List[Channel](Quote(GInt(7)), Quote(GInt(8)), Quote(GInt(9))),
                                     false))
            ),
            None
          )
      ))
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    val result = withTestStore(testStore) { store =>
      val receive =
        Receive(Seq(
                  ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                              Quote(GString("channel")))),
                Par(),
                false,
                3,
                0,
                BitSet())
      val interpreter = Reduce.makeInterpreter(store)
      val resultTask  = interpreter.eval(receive)(Env())
      val inspectTask = for {
        _ <- resultTask
      } yield store.toHashMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("channel")))) ->

          Row(
            None,
            Some(
              List(
                WaitingContinuation[List[Channel], Par](List(List(Channel(ChanVar(FreeVar(0))),
                                                                  Channel(ChanVar(FreeVar(1))),
                                                                  Channel(ChanVar(FreeVar(2))))),
                                                        Par(),
                                                        false)
              )
            )
          )
      ))
  }

  "eval of Send | Receive" should "meet in the tuplespace and proceed." in {
    val send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel")))),
      Send(Quote(GString("result")), List(GString("Success")), false, 0, BitSet()),
      false,
      3,
      0,
      BitSet()
    )
    val sendFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )

    val receiveFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )
  }

  "eval of Send on (7 + 8) | Receive on 15" should "meet in the tuplespace and proceed." in {
    val send =
      Send(Quote(EPlus(GInt(7), GInt(8))), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GInt(15)))),
      Send(Quote(GString("result")), List(GString("Success")), false, 0, BitSet()),
      false,
      3,
      0,
      BitSet()
    )

    val sendFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )

    val receiveFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )
  }

  "Simple match" should "capture and add to the environment." in {
    val result = withTestStore(testStore) { store =>
      val pattern = Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, 2, BitSet())
      val sendTarget =
        Send(ChanVar(BoundVar(1)), List(GInt(7), EVar(BoundVar(0))), false, 0, BitSet(0, 1))
      val matchTerm = Match(sendTarget,
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
      val interpreter = Reduce.makeInterpreter(store)
      val matchTask   = interpreter.eval(matchTerm)(env)
      val inspectTask = for {
        _ <- matchTask
      } yield store.toHashMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(
            Some(
              List(
                Datum[List[Channel]](List[Channel](Quote(GPrivate("one")), Quote(GPrivate("zero"))),
                                     false))),
            None
          )
      )
    )
  }

  "eval of Send | Send | Receive join" should "meet in the tuplespace and proceed." in {
    val send1 =
      Send(Quote(GString("channel1")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val send2 =
      Send(Quote(GString("channel2")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel1"))),
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel2")))
      ),
      Send(Quote(GString("result")), List(GString("Success")), false, 0, BitSet()),
      false,
      3,
      0,
      BitSet()
    )
    val sendFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(send2)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )

    val receiveFirstResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(send2)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )

    val interleavedResult = withTestStore(testStore) { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskInterleaved = for {
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send2)(Env())
      } yield store.toHashMap
      Await.result(inspectTaskInterleaved.runAsync, 3.seconds)
    }
    interleavedResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(Some(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false))),
              None)
      )
    )
  }
}
