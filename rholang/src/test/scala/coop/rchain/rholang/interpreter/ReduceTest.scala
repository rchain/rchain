package coop.rchain.rholang.interpreter

import java.nio.file.Files

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
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rspace.{IStore, LMDBStore, Serialize}
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import cats.syntax.either._

trait PersistentStoreTester {
  implicit val serializer = Serialize.mkProtobufInstance(Channel)
  implicit val serializer2 = new Serialize[Seq[Channel]] {
    override def encode(a: Seq[Channel]): Array[Byte] =
      ListChannel.toByteArray(ListChannel(a))
    override def decode(bytes: Array[Byte]): Either[Throwable, Seq[Channel]] =
      Either.catchNonFatal(ListChannel.parseFrom(bytes).channels.toList)
  }
  implicit val serializer3 = Serialize.mkProtobufInstance(Par)
  def withTestStore[R](f: IStore[Channel, Seq[Channel], Seq[Channel], Par] => R): R = {
    val dbDir = Files.createTempDirectory("rchain-storage-test-")
    val store: IStore[Channel, Seq[Channel], Seq[Channel], Par] =
      LMDBStore.create[Channel, Seq[Channel], Seq[Channel], Par](dbDir, 1024 * 1024 * 1024)
    try {
      f(store)
    } finally {
      store.close()
    }
  }
}

class ReduceSpec extends FlatSpec with Matchers with PersistentStoreTester {

  "evalExpr" should "handle simple addition" in {

    val result = withTestStore { store =>
      val addExpr    = EPlus(GInt(7), GInt(8))
      val resultTask = Reduce.makeInterpreter(store).evalExpr(addExpr)(Env())
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected: Expr = GInt(15)
    result should be(expected)
  }

  "evalExpr" should "leave ground values alone" in {
    val result = withTestStore { store =>
      val groundExpr = GInt(7)
      val resultTask = Reduce.makeInterpreter(store).evalExpr(groundExpr)(Env())
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected: Expr = GInt(7)
    result should be(expected)
  }

  "eval of Send" should "place something in the tuplespace." in {
    val result = withTestStore { store =>
      val send =
        Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, 0, BitSet())
      val interpreter = Reduce.makeInterpreter(store)
      val resultTask  = interpreter.eval(send)(Env())
      val inspectTask = for {
        _ <- resultTask
      } yield store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("channel")))) ->
          Row(
            List(Datum[List[Channel]](List[Channel](Quote(GInt(7)), Quote(GInt(8)), Quote(GInt(9))),
                                      false)),
            List()
          )
      ))
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    val result = withTestStore { store =>
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
      } yield store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("channel")))) ->

          Row(
            List(),
            List(
              WaitingContinuation[List[Channel], Par](List(List(Channel(ChanVar(FreeVar(0))),
                                                                Channel(ChanVar(FreeVar(1))),
                                                                Channel(ChanVar(FreeVar(2))))),
                                                      Par(),
                                                      false)
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
    val sendFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )

    val receiveFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send)(Env())
      } yield store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
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

    val sendFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )

    val receiveFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send)(Env())
      } yield store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )
  }

  "Simple match" should "capture and add to the environment." in {
    val result = withTestStore { store =>
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
      } yield store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    result should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(
            List(
              Datum[List[Channel]](List[Channel](Quote(GPrivate("one")), Quote(GPrivate("zero"))),
                                   false)),
            List()
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
    val sendFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(send2)(Env())
        _ <- interpreter.eval(receive)(Env())
      } yield store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }
    sendFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )

    val receiveFirstResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskReceiveFirst = for {
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(send2)(Env())
      } yield store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )

    val interleavedResult = withTestStore { store =>
      val interpreter = Reduce.makeInterpreter(store)
      val inspectTaskInterleaved = for {
        _ <- interpreter.eval(send1)(Env())
        _ <- interpreter.eval(receive)(Env())
        _ <- interpreter.eval(send2)(Env())
      } yield store.toMap
      Await.result(inspectTaskInterleaved.runAsync, 3.seconds)
    }
    interleavedResult should be(
      HashMap(
        List(Channel(Quote(GString("result")))) ->
          Row(List(Datum[List[Channel]](List[Channel](Quote(GString("Success"))), false)), List())
      )
    )
  }
}
