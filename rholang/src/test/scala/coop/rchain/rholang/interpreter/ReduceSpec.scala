package coop.rchain.rholang.interpreter

import java.nio.file.Files

import cats.mtl.FunctorTell
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Capture._
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.{GPrivate => _, _}
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.errors.ReduceError
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.collection.mutable.HashMap
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionException}

trait PersistentStoreTester {
  def withTestSpace[R](
      f: ISpace[Channel, BindPattern, Seq[Channel], TaggedContinuation] => R): R = {
    val dbDir = Files.createTempDirectory("rchain-storage-test-")
    val store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation] =
      LMDBStore.create[Channel, BindPattern, Seq[Channel], TaggedContinuation](dbDir,
                                                                               1024 * 1024 * 1024)
    val space = new RSpace(store, Branch("test"))
    try {
      f(space)
    } finally {
      space.close()
    }
  }
}

class ReduceSpec extends FlatSpec with Matchers with PersistentStoreTester {
  "evalExpr" should "handle simple addition" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val addExpr      = EPlus(GInt(7), GInt(8))
      implicit val env = Env[Par]()
      val resultTask   = reducer.evalExpr(addExpr)
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected = Seq(Expr(GInt(15)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "leave ground values alone" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val groundExpr   = GInt(7)
      implicit val env = Env[Par]()
      val resultTask   = reducer.evalExpr(groundExpr)
      Await.result(resultTask.runAsync, 3.seconds)
    }

    val expected = Seq(Expr(GInt(7)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "handle equality between arbitary processes" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val eqExpr       = EEq(GPrivate("private_name"), GPrivate("private_name"))
      implicit val env = Env[Par]()
      val resultTask   = reducer.evalExpr(eqExpr)
      Await.result(resultTask.runAsync, 3.seconds)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "substitute before comparison." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer           = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val emptyEnv = Env.makeEnv(Par(), Par())
      val eqExpr            = EEq(EVar(BoundVar(0)), EVar(BoundVar(1)))
      val resultTask        = reducer.evalExpr(eqExpr)
      Await.result(resultTask.runAsync, 3.seconds)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Bundle" should "evaluate contents of bundle" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val bundleSend =
        Bundle(Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, BitSet()))
      val interpreter  = reducer
      implicit val env = Env[Par]()
      val resultTask   = interpreter.eval(bundleSend)
      val inspectTask = for {
        _ <- resultTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("channel")))

    result should be(
      HashMap(
        List(channel) ->
          Row(
            List(
              Datum.create(channel,
                           Seq[Channel](Quote(GInt(7)), Quote(GInt(8)), Quote(GInt(9))),
                           false)),
            List()
          )
      ))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  // Await.result wraps any Exception with ExecutionException so we need to catch it, unwrap and rethrow
  def runAndRethrowBoxedErrors[A](task: Task[A], timeout: Duration = 3.seconds): A =
    try {
      Await.result(task.runAsync, timeout)
    } catch {
      case boxedError: ExecutionException =>
        throw boxedError.getCause
    }

  it should "throw an error if names are used against their polarity" in {
    implicit val errorLog = new Runtime.ErrorLog()
    /* for (n <- @bundle+ { y } ) { }  -> for (n <- y) { }
     */
    val y = GString("y")
    val receive = Receive(
      Seq(ReceiveBind(Seq(Quote(Par())), Quote(Bundle(y, readFlag = false, writeFlag = true)))),
      Par())

    val receiveResult = withTestSpace { space =>
      implicit val env = Env[Par]()
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val task         = reducer.eval(receive).map(_ => space.store.toMap)
      Await.result(task.runAsync, 3.seconds)
    }
    receiveResult should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(ReduceError("Trying to read from non-readable channel.")))

    /* @bundle- { x } !(7) -> x!(7)
     */
    val x = GString("channel")
    val send =
      Send(Channel(Quote(Bundle(x, writeFlag = false, readFlag = true))), Seq(Expr(GInt(7))))

    val sendResult = withTestSpace { space =>
      implicit val env = Env[Par]()
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val task         = reducer.eval(send).map(_ => space.store.toMap)
      Await.result(task.runAsync, 3.seconds)
    }
    sendResult should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(ReduceError("Trying to send on non-writeable channel.")))
  }

  "eval of Send" should "place something in the tuplespace." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val reducer = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val send =
        Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
      val interpreter  = reducer
      implicit val env = Env[Par]()
      val resultTask   = interpreter.eval(send)
      val inspectTask = for {
        _ <- resultTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("channel")))

    result should be(
      HashMap(
        List(channel) ->
          Row(
            List(
              Datum.create(channel,
                           Seq[Channel](Quote(GInt(7)), Quote(GInt(8)), Quote(GInt(9))),
                           false)),
            List()
          )
      ))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "verify that Bundle is writeable before sending on Bundle " in {
    implicit val errorLog = new Runtime.ErrorLog()
    /* @bundle+ { x } !(7) -> x!(7)
     */
    val x = GString("channel")
    val send =
      Send(Channel(Quote(Bundle(x, writeFlag = true, readFlag = false))), Seq(Expr(GInt(7))))

    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val task         = reducer.eval(send).map(_ => space.store.toMap)
      Await.result(task.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(x))

    result should be(
      HashMap(List(channel) -> Row(List(Datum.create(channel, Seq[Channel](Quote(GInt(7))), false)),
                                   List())))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val receive =
        Receive(Seq(
                  ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                              Quote(GString("channel")))),
                Par(),
                false,
                3,
                BitSet())
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val interpreter  = reducer
      implicit val env = Env[Par]()
      val resultTask   = interpreter.eval(receive)
      val inspectTask = for {
        _ <- resultTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channels = List(Channel(Quote(GString("channel"))))

    result should be(
      HashMap(
        channels ->
          Row(
            List(),
            List(
              WaitingContinuation
                .create[Channel, BindPattern, TaggedContinuation](
                  channels,
                  List(
                    BindPattern(List(Channel(ChanVar(FreeVar(0))),
                                     Channel(ChanVar(FreeVar(1))),
                                     Channel(ChanVar(FreeVar(2)))),
                                None)),
                  TaggedContinuation(ParBody(Par())),
                  false
                )
            )
          )
      ))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "verify that bundle is readable if receiving on Bundle" in {
    implicit val errorLog = new Runtime.ErrorLog()
    /* for (@Nil <- @bundle- { y } ) { }  -> for (n <- y) { }
     */

    val y = GString("y")
    val receive = Receive(binds = Seq(
                            ReceiveBind(
                              patterns = Seq(Quote(Par())),
                              source = Quote(Bundle(y, readFlag = true, writeFlag = false))
                            )),
                          body = Par())

    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val task         = reducer.eval(receive).map(_ => space.store.toMap)
      Await.result(task.runAsync, 3.seconds)
    }

    val channels = List(Channel(Quote(y)))

    result should be(
      HashMap(
        channels ->
          Row(
            List(),
            List(
              WaitingContinuation.create[Channel, BindPattern, TaggedContinuation](
                channels,
                List(BindPattern(List(Channel(Quote(Par()))), None)),
                TaggedContinuation(ParBody(Par())),
                false))
          )
      ))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send | Receive" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel")),
                    freeCount = 3)),
      Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )
    val sendFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskSendFirst = for {
        _ <- reducer.eval(send)
        _ <- reducer.eval(receive)
      } yield space.store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    sendFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskReceiveFirst = for {
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send)
      } yield space.store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }

    receiveFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send on (7 + 8) | Receive on 15" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val send =
      Send(Quote(EPlus(GInt(7), GInt(8))), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GInt(15)),
                    freeCount = 3)),
      Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )

    val sendFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskSendFirst = for {
        _ <- reducer.eval(send)
        _ <- reducer.eval(receive)
      } yield space.store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    sendFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskReceiveFirst = for {
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send)
      } yield space.store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send of Receive | Receive" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val simpleReceive = Receive(
      Seq(ReceiveBind(Seq(Quote(GInt(2))), Quote(GInt(2)))),
      Par(),
      false,
      0,
      BitSet()
    )
    val send =
      Send(Quote(GInt(1)), Seq[Par](simpleReceive), false, BitSet())
    val receive = Receive(
      Seq(ReceiveBind(Seq(ChanVar(FreeVar(0))), Quote(GInt(1)), freeCount = 1)),
      EEvalBody(ChanVar(BoundVar(0))),
      false,
      1,
      BitSet()
    )

    val sendFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val interpreter  = reducer
      implicit val env = Env[Par]()
      val inspectTaskSendFirst = for {
        _ <- interpreter.eval(send)
        _ <- interpreter.eval(receive)
      } yield space.store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }

    val channels = List(Channel(Quote(GInt(2))))

    sendFirstResult should be(
      HashMap(
        channels ->
          Row(
            List(),
            List(
              WaitingContinuation.create[Channel, BindPattern, TaggedContinuation](
                channels,
                List(BindPattern(List(Quote(GInt(2))))),
                TaggedContinuation(ParBody(Par())),
                false)
            )
          )
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskReceiveFirst = for {
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send)
      } yield space.store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        channels ->
          Row(
            List(),
            List(
              WaitingContinuation.create[Channel, BindPattern, TaggedContinuation](
                channels,
                List(BindPattern(List(Quote(GInt(2))))),
                TaggedContinuation(ParBody(Par())),
                false))
          )
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Simple match" should "capture and add to the environment." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val result = withTestSpace { space =>
      val pattern = Send(ChanVar(FreeVar(0)), List(GInt(7), EVar(FreeVar(1))), false, BitSet())
        .withConnectiveUsed(true)
      val sendTarget =
        Send(ChanVar(BoundVar(1)), List(GInt(7), EVar(BoundVar(0))), false, BitSet(0, 1))
      val matchTerm = Match(
        sendTarget,
        List(
          MatchCase(
            pattern,
            Send(Quote(GString("result")),
                 List(EEvalBody(ChanVar(BoundVar(1))), EVar(BoundVar(0))),
                 false,
                 BitSet(0, 1)),
            freeCount = 2
          )),
        BitSet()
      )
      implicit val env = Env.makeEnv[Par](GPrivate("one"), GPrivate("zero"))
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val matchTask    = reducer.eval(matchTerm)
      val inspectTask = for {
        _ <- matchTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(
            List(
              Datum.create(channel,
                           Seq[Channel](Quote(GPrivate("one")), Quote(GPrivate("zero"))),
                           false)),
            List()
          )
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send | Send | Receive join" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val send1 =
      Send(Quote(GString("channel1")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val send2 =
      Send(Quote(GString("channel2")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel1")),
                    freeCount = 3),
        ReceiveBind(Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1)), ChanVar(FreeVar(2))),
                    Quote(GString("channel2")),
                    freeCount = 3)
      ),
      Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )
    val sendFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskSendFirst = for {
        _ <- reducer.eval(send1)
        _ <- reducer.eval(send2)
        _ <- reducer.eval(receive)
      } yield space.store.toMap
      Await.result(inspectTaskSendFirst.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    sendFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskReceiveFirst = for {
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send1)
        _ <- reducer.eval(send2)
      } yield space.store.toMap
      Await.result(inspectTaskReceiveFirst.runAsync, 3.seconds)
    }
    receiveFirstResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val interleavedResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val inspectTaskInterleaved = for {
        _ <- reducer.eval(send1)
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send2)
      } yield space.store.toMap
      Await.result(inspectTaskInterleaved.runAsync, 3.seconds)
    }
    interleavedResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send with remainder receive" should "capture the remainder." in {
    implicit val errorLog = new Runtime.ErrorLog()
    val send =
      Send(Quote(GString("channel")), List(GInt(7), GInt(8), GInt(9)), false, BitSet())
    val receive =
      Receive(Seq(ReceiveBind(Seq(), Quote(GString("channel")), Some(FreeVar(0)), freeCount = 1)),
              Send(Quote(GString("result")), Seq(EVar(BoundVar(0)))))

    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val task = for {
        _ <- reducer.eval(receive)
        _ <- reducer.eval(send)
      } yield space.store.toMap
      Await.result(task.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(List(
                Datum.create(channel,
                             Seq[Channel](Quote(EList(List(GInt(7), GInt(8), GInt(9))))),
                             false)),
              List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of nth method" should "pick out the nth item from a list" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val nthCall: Expr =
      EMethod("nth", EList(List(GInt(7), GInt(8), GInt(9), GInt(10))), List[Par](GInt(2)))
    val directResult: Par = withTestSpace { space =>
      implicit val env = Env[Par]()
      val reducer =
        RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      Await.result(reducer.evalExprToPar(nthCall).runAsync, 3.seconds)
    }
    val expectedResult: Par = GInt(9)
    directResult should be(expectedResult)

    val nthCallEvalToSend: Expr =
      EMethod("nth",
              EList(
                List(GInt(7),
                     Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
                     GInt(9),
                     GInt(10))),
              List[Par](GInt(1)))
    val indirectResult = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val nthTask      = reducer.eval(nthCallEvalToSend)
      val inspectTask = for {
        _ <- nthTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    indirectResult should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("Success"))), false)), List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of nth method in send position" should "change what is sent" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val nthCallEvalToSend: Expr =
      EMethod("nth",
              EList(
                List(GInt(7),
                     Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
                     GInt(9),
                     GInt(10))),
              List[Par](GInt(1)))
    val send: Par =
      Send(Quote(GString("result")), List[Par](nthCallEvalToSend), false, BitSet())
    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val nthTask      = reducer.eval(send)
      val inspectTask = for {
        _ <- nthTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(
            List(
              Datum.create(
                channel,
                Seq[Channel](
                  Quote(Send(Quote(GString("result")), List(GString("Success")), false, BitSet()))),
                false)),
            List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of `toByteArray` method on any process" should "return that process serialized" in {
    implicit val errorLog = new Runtime.ErrorLog()
    import coop.rchain.models.serialization.implicits._
    val proc = Receive(Seq(ReceiveBind(Seq(ChanVar(FreeVar(0))), Quote(GString("channel")))),
                       Par(),
                       false,
                       1,
                       BitSet())
    val serializedProcess         = com.google.protobuf.ByteString.copyFrom(Serialize[Par].encode(proc).toArray)
    val toByteArrayCall           = EMethod("toByteArray", proc, List[Par]())
    def wrapWithSend(p: Par): Par = Send(Quote(GString("result")), List[Par](p), false, BitSet())
    val result = withTestSpace { space =>
      val reducer     = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val env         = Env[Par]()
      val task        = reducer.eval(wrapWithSend(toByteArrayCall))(env)
      val inspectTask = for { _ <- task } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(List(
                Datum.create(channel,
                             Seq[Channel](Quote(Expr(GByteArray(serializedProcess)))),
                             persist = false)),
              List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "return an error when `toByteArray` is called with arguments" in {
    implicit val errorLog = new Runtime.ErrorLog()
    val toByteArrayWithArgumentsCall: EMethod =
      EMethod(
        "toByteArray",
        Par(sends = Seq(Send(Quote(GString("result")), List(GString("Success")), false, BitSet()))),
        List[Par](GInt(1)))

    val result = withTestSpace { space =>
      val reducer      = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      implicit val env = Env[Par]()
      val nthTask      = reducer.eval(toByteArrayWithArgumentsCall)
      val inspectTask = for {
        _ <- nthTask
      } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }
    result should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(ReduceError("Error: toByteArray does not take arguments")))
  }

  "eval of hexToBytes" should "transform encoded string to byte array (not the rholang term)" in {
    import coop.rchain.models.serialization.implicits._
    implicit val errorLog         = new Runtime.ErrorLog()
    val testString                = "testing testing"
    val base16Repr                = Base16.encode(testString.getBytes)
    val proc: Par                 = GString(base16Repr)
    val serializedProcess         = com.google.protobuf.ByteString.copyFrom(Serialize[Par].encode(proc).toArray)
    val toByteArrayCall           = EMethod("hexToBytes", proc, List[Par]())
    def wrapWithSend(p: Par): Par = Send(Quote(GString("result")), List[Par](p), false, BitSet())
    val result = withTestSpace { space =>
      val reducer     = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val env         = Env[Par]()
      val task        = reducer.eval(wrapWithSend(toByteArrayCall))(env)
      val inspectTask = for { _ <- task } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(List(
                Datum.create(channel,
                  Seq[Channel](Quote(Expr(GByteArray(ByteString.copyFrom(testString.getBytes))))),
                  persist = false)),
              List())
      )
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "variable references" should "be substituted before being used." in {
    val proc = New(
      bindCount = 1,
      p = Par(
        sends = List(
          Send(chan = Channel(ChanVar(BoundVar(0))),
               data = List(EEvalBody(ChanVar(BoundVar(0)))),
               persistent = false)),
        receives = List(
          Receive(
            binds = List(
              ReceiveBind(patterns = List(Quote(Connective(VarRefBody(VarRef(0, 1))))),
                          source = ChanVar(BoundVar(0)),
                          freeCount = 0)),
            body = Send(chan = Quote(GString("result")), data = List(GString("true"))),
            bindCount = 0
          ))
      )
    )

    val result = withTestSpace { space =>
      implicit val errorLog = new Runtime.ErrorLog()
      val reducer           = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val env               = Env[Par]()
      val task              = reducer.eval(proc)(env)
      val inspectTask       = for { _ <- task } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("true"))), persist = false)),
              List())
      )
    )
  }
  it should "be substituted before being used in a match." in {
    val proc = New(
      bindCount = 1,
      p = Match(
        target = EVar(BoundVar(0)),
        cases = List(
          MatchCase(pattern = Connective(VarRefBody(VarRef(0, 1))),
                    source = Send(chan = Quote(GString("result")), data = List(GString("true")))))
      )
    )

    val result = withTestSpace { space =>
      implicit val errorLog = new Runtime.ErrorLog()
      val reducer           = RholangOnlyDispatcher.create[Task, Task.Par](space).reducer
      val env               = Env[Par]()
      val task              = reducer.eval(proc)(env)
      val inspectTask       = for { _ <- task } yield space.store.toMap
      Await.result(inspectTask.runAsync, 3.seconds)
    }

    val channel = Channel(Quote(GString("result")))

    result should be(
      HashMap(
        List(channel) ->
          Row(List(Datum.create(channel, Seq[Channel](Quote(GString("true"))), persist = false)),
              List())
      )
    )
  }
}
