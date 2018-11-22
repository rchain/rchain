package coop.rchain.rholang.interpreter

import java.nio.file.Files

import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{Assertion, FlatSpec, Matchers}
import coop.rchain.shared.PathOps._

import scala.collection.immutable.BitSet
import scala.collection.mutable.HashMap
import scala.concurrent.Await
import scala.concurrent.duration._

final case class TestFixture(space: RhoISpace[Task], reducer: ChargingReducer[Task])

trait PersistentStoreTester {
  def withTestSpace[R](errorLog: ErrorLog)(f: TestFixture => R): R = {
    val dbDir               = Files.createTempDirectory("rholang-interpreter-test-")
    val context: RhoContext = Context.create(dbDir, mapSize = 1024L * 1024L * 1024L)
    val space = (RSpace
      .create[
        Task,
        Par,
        BindPattern,
        OutOfPhlogistonsError.type,
        ListParWithRandom,
        ListParWithRandomAndPhlos,
        TaggedContinuation
      ](context, Branch("test")))
      .unsafeRunSync
    implicit val errLog = errorLog
    val reducer         = RholangOnlyDispatcher.create[Task, Task.Par](space)._2
    reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
    try {
      f(TestFixture(space, reducer))
    } finally {
      space.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}

class ReduceSpec extends FlatSpec with Matchers with PersistentStoreTester {
  implicit val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])

  def checkData(
      result: Map[
        scala.collection.immutable.Seq[Par],
        Row[BindPattern, ListParWithRandom, TaggedContinuation]
      ]
  )(channel: Par, data: Seq[Par], rand: Blake2b512Random): Assertion =
    result should be(
      HashMap(
        List(channel) ->
          Row(
            List(
              Datum.create(
                channel,
                ListParWithRandom(
                  data,
                  rand
                ),
                false
              )
            ),
            List()
          )
      )
    )

  def checkContinuation(
      result: Map[
        scala.collection.immutable.Seq[Par],
        Row[BindPattern, ListParWithRandom, TaggedContinuation]
      ]
  )(channels: List[Par], bindPatterns: List[BindPattern], body: ParWithRandom): Assertion =
    result should be(
      HashMap(
        channels ->
          Row(
            List(),
            List(
              WaitingContinuation.create[Par, BindPattern, TaggedContinuation](
                channels,
                bindPatterns,
                TaggedContinuation(ParBody(body)),
                false
              )
            )
          )
      )
    )

  "evalExpr" should "handle simple addition" in {
    implicit val errorLog = new ErrorLog()
    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        val addExpr      = EPlus(GInt(7L), GInt(8L))
        implicit val env = Env[Par]()
        val resultTask   = reducer.evalExpr(addExpr)
        Await.result(resultTask.runToFuture, 3.seconds)
    }

    val expected = Seq(Expr(GInt(15L)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "handle long addition" in {
    implicit val errorLog = new ErrorLog()
    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        val addExpr      = EPlus(GInt(Int.MaxValue), GInt(Int.MaxValue))
        implicit val env = Env[Par]()
        val resultTask   = reducer.evalExpr(addExpr)
        Await.result(resultTask.runToFuture, 3.seconds)
    }

    val expected = Seq(Expr(GInt(2 * Int.MaxValue.toLong)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "leave ground values alone" in {
    implicit val errorLog = new ErrorLog()
    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        val groundExpr   = GInt(7L)
        implicit val env = Env[Par]()
        val resultTask   = reducer.evalExpr(groundExpr)
        Await.result(resultTask.runToFuture, 3.seconds)
    }

    val expected = Seq(Expr(GInt(7L)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "handle equality between arbitary processes" in {
    implicit val errorLog = new ErrorLog()
    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        val eqExpr       = EEq(GPrivateBuilder("private_name"), GPrivateBuilder("private_name"))
        implicit val env = Env[Par]()
        val resultTask   = reducer.evalExpr(eqExpr)
        Await.result(resultTask.runToFuture, 3.seconds)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "evalExpr" should "substitute before comparison." in {
    implicit val errorLog = new ErrorLog()
    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val emptyEnv = Env.makeEnv(Par(), Par())
        val eqExpr            = EEq(EVar(BoundVar(0)), EVar(BoundVar(1)))
        val resultTask        = reducer.evalExpr(eqExpr)
        Await.result(resultTask.runToFuture, 3.seconds)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Bundle" should "evaluate contents of bundle" in {
    implicit val errorLog = new ErrorLog()
    val splitRand         = rand.splitByte(0)
    val channel: Par      = GString("channel")
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val bundleSend =
          Bundle(Send(channel, List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet()))
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(bundleSend)(env, splitRand)
        val inspectTask = for {
          _ <- resultTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    checkData(result)(channel, Seq(GInt(7L), GInt(8L), GInt(9L)), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "throw an error if names are used against their polarity" in {
    implicit val errorLog = new ErrorLog()

    /* for (n <- @bundle+ { y } ) { }  -> for (n <- y) { }
     */
    val y = GString("y")
    val receive = Receive(
      Seq(ReceiveBind(Seq(Par()), Bundle(y, readFlag = false, writeFlag = true))),
      Par()
    )

    val receiveResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val task         = reducer.eval(receive).map(_ => space.store.toMap)
        Await.result(task.runToFuture, 3.seconds)
    }
    receiveResult should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(ReduceError("Trying to read from non-readable channel."))
    )

    /* @bundle- { x } !(7) -> x!(7)
     */
    val x = GString("channel")
    val send =
      Send(Bundle(x, writeFlag = false, readFlag = true), Seq(Expr(GInt(7L))))

    val sendResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()

        val task = reducer.eval(send).map(_ => space.store.toMap)
        Await.result(task.runToFuture, 3.seconds)
    }
    sendResult should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(ReduceError("Trying to send on non-writeable channel."))
    )
  }

  "eval of Send" should "place something in the tuplespace." in {
    implicit val errorLog = new ErrorLog()
    val channel: Par      = GString("channel")
    val splitRand         = rand.splitByte(0)
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val send =
          Send(channel, List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(send)(env, splitRand)
        val inspectTask = for {
          _ <- resultTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    checkData(result)(channel, Seq(GInt(7L), GInt(8L), GInt(9L)), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "verify that Bundle is writeable before sending on Bundle " in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    /* @bundle+ { x } !(7) -> x!(7)
     */
    val channel = GString("channel")
    val send =
      Send(Bundle(channel, writeFlag = true, readFlag = false), Seq(Expr(GInt(7L))))

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val task         = reducer.eval(send)(env, splitRand).map(_ => space.store.toMap)
        Await.result(task.runToFuture, 3.seconds)
    }

    checkData(result)(channel, Seq(GInt(7L)), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    implicit val errorLog = new ErrorLog()

    val splitRand    = rand.splitByte(0)
    val channel: Par = GString("channel")
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val receive =
          Receive(
            Seq(
              ReceiveBind(
                Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
                channel
              )
            ),
            Par(),
            false,
            3,
            BitSet()
          )
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(receive)(env, splitRand)
        val inspectTask = for {
          _ <- resultTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val bindPattern = BindPattern(
      List(
        EVar(FreeVar(0)),
        EVar(FreeVar(1)),
        EVar(FreeVar(2))
      ),
      None
    )
    checkContinuation(result)(List(channel), List(bindPattern), ParWithRandom(Par(), splitRand))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "verify that bundle is readable if receiving on Bundle" in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(1)
    /* for (@Nil <- @bundle- { y } ) { }  -> for (n <- y) { }
     */

    val y = GString("y")
    val receive = Receive(
      binds = Seq(
        ReceiveBind(
          patterns = Seq(Par()),
          source = Bundle(y, readFlag = true, writeFlag = false)
        )
      ),
      body = Par()
    )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val task         = reducer.eval(receive)(env, splitRand).map(_ => space.store.toMap)
        Await.result(task.runToFuture, 3.seconds)
    }

    val channels = List[Par](y)
    checkContinuation(result)(
      channels,
      List(BindPattern(List(Par()), None)),
      ParWithRandom(Par(), splitRand)
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send | Receive" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new ErrorLog()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(GString("channel"), List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GString("channel"),
          freeCount = 3
        )
      ),
      Send(GString("result"), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )
    val sendFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskSendFirst = for {
          _ <- reducer.eval(send)(env, splitRand0)
          _ <- reducer.eval(receive)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskSendFirst.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(sendFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(receive)(env, splitRand1)
          _ <- reducer.eval(send)(env, splitRand0)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }

    checkData(receiveFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send | Receive" should "when whole list is bound to list remainder, meet in the tuplespace and proceed. (RHOL-422)" in {
    // for(@[...a] <- @"channel") { … } | @"channel"!([7,8,9])
    implicit val errorLog = new ErrorLog()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    // format: off
    val send =
      Send(GString("channel"), List(Par(exprs = Seq(Expr(EListBody(EList(Seq(GInt(7L), GInt(8L), GInt(9L)))))))), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(Par(exprs = Seq(EListBody(EList(connectiveUsed = true, remainder = Some(FreeVar(0))))))),
          GString("channel"),
          freeCount = 1)),
      Send(GString("result"), List(GString("Success")), false, BitSet()),
      false,
      1,
      BitSet()
    )
    // format: on
    val sendFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskSendFirst = for {
          _ <- reducer.eval(send)(env, splitRand0)
          _ <- reducer.eval(receive)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskSendFirst.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(sendFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(receive)(env, splitRand1)
          _ <- reducer.eval(send)(env, splitRand0)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }

    checkData(receiveFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send on (7 + 8) | Receive on 15" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new ErrorLog()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(EPlus(GInt(7L), GInt(8L)), List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GInt(15L),
          freeCount = 3
        )
      ),
      Send(GString("result"), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )

    val sendFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskSendFirst = for {
          _ <- reducer.eval(send)(env, splitRand0)
          _ <- reducer.eval(receive)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskSendFirst.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(sendFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(receive)(env, splitRand1)
          _ <- reducer.eval(send)(env, splitRand0)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }
    checkData(receiveFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send of Receive | Receive" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new ErrorLog()

    val baseRand   = rand.splitByte(2)
    val splitRand0 = baseRand.splitByte(0)
    val splitRand1 = baseRand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val simpleReceive = Receive(
      Seq(ReceiveBind(Seq(GInt(2L)), GInt(2L))),
      Par(),
      false,
      0,
      BitSet()
    )
    val send =
      Send(GInt(1L), Seq[Par](simpleReceive), false, BitSet())
    val receive = Receive(
      Seq(ReceiveBind(Seq(EVar(FreeVar(0))), GInt(1L), freeCount = 1)),
      EVar(BoundVar(0)),
      false,
      1,
      BitSet()
    )

    val sendFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskSendFirst = for {
          _ <- reducer.eval(send)(env, splitRand0)
          _ <- reducer.eval(receive)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskSendFirst.runToFuture, 3.seconds)
    }

    val channels = List[Par](GInt(2L))

    // Because they are evaluated separately, nothing is split.
    checkContinuation(sendFirstResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(receive)(env, splitRand1)
          _ <- reducer.eval(send)(env, splitRand0)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }

    checkContinuation(receiveFirstResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val bothResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(Par(receives = Seq(receive), sends = Seq(send)))(env, baseRand)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }

    checkContinuation(bothResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Simple match" should "capture and add to the environment." in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val pattern = Send(EVar(FreeVar(0)), List(GInt(7L), EVar(FreeVar(1))), false, BitSet())
          .withConnectiveUsed(true)
        val sendTarget =
          Send(EVar(BoundVar(1)), List(GInt(7L), EVar(BoundVar(0))), false, BitSet(0, 1))
        val matchTerm = Match(
          sendTarget,
          List(
            MatchCase(
              pattern,
              Send(
                GString("result"),
                List(EVar(BoundVar(1)), EVar(BoundVar(0))),
                false,
                BitSet(0, 1)
              ),
              freeCount = 2
            )
          ),
          BitSet()
        )
        implicit val env = Env.makeEnv[Par](GPrivateBuilder("one"), GPrivateBuilder("zero"))

        val matchTask = reducer.eval(matchTerm)(env, splitRand)
        val inspectTask = for {
          _ <- matchTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(result)(channel, Seq(GPrivateBuilder("one"), GPrivateBuilder("zero")), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send | Send | Receive join" should "meet in the tuplespace and proceed." in {
    implicit val errorLog = new ErrorLog()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val splitRand2 = rand.splitByte(2)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand2, splitRand0, splitRand1))
    val send1 =
      Send(GString("channel1"), List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val send2 =
      Send(GString("channel2"), List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GString("channel1"),
          freeCount = 3
        ),
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GString("channel2"),
          freeCount = 3
        )
      ),
      Send(GString("result"), List(GString("Success")), false, BitSet()),
      false,
      3,
      BitSet()
    )
    val sendFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskSendFirst = for {
          _ <- reducer.eval(send1)(env, splitRand0)
          _ <- reducer.eval(send2)(env, splitRand1)
          _ <- reducer.eval(receive)(env, splitRand2)
        } yield space.store.toMap
        Await.result(inspectTaskSendFirst.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(sendFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val receiveFirstResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskReceiveFirst = for {
          _ <- reducer.eval(receive)(env, splitRand2)
          _ <- reducer.eval(send1)(env, splitRand0)
          _ <- reducer.eval(send2)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskReceiveFirst.runToFuture, 3.seconds)
    }

    checkData(receiveFirstResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])

    val interleavedResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val inspectTaskInterleaved = for {
          _ <- reducer.eval(send1)(env, splitRand0)
          _ <- reducer.eval(receive)(env, splitRand2)
          _ <- reducer.eval(send2)(env, splitRand1)
        } yield space.store.toMap
        Await.result(inspectTaskInterleaved.runToFuture, 3.seconds)
    }

    checkData(interleavedResult)(channel, Seq(GString("Success")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of Send with remainder receive" should "capture the remainder." in {
    implicit val errorLog = new ErrorLog()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(GString("channel"), List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val receive =
      Receive(
        Seq(ReceiveBind(Seq(), GString("channel"), Some(FreeVar(0)), freeCount = 1)),
        Send(GString("result"), Seq(EVar(BoundVar(0))))
      )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val task = for {
          _ <- reducer.eval(receive)(env, splitRand1)
          _ <- reducer.eval(send)(env, splitRand0)
        } yield space.store.toMap
        Await.result(task.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(result)(channel, Seq(EList(List(GInt(7L), GInt(8L), GInt(9L)))), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of nth method" should "pick out the nth item from a list" in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    val nthCall: Expr =
      EMethod("nth", EList(List(GInt(7L), GInt(8L), GInt(9L), GInt(10L))), List[Par](GInt(2L)))
    val directResult: Par = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env[Par]()
        Await.result(reducer.evalExprToPar(nthCall).runToFuture, 3.seconds)
    }
    val expectedResult: Par = GInt(9L)
    directResult should be(expectedResult)

    val nthCallEvalToSend: Expr =
      EMethod(
        "nth",
        EList(
          List(
            GInt(7L),
            Send(GString("result"), List(GString("Success")), false, BitSet()),
            GInt(9L),
            GInt(10L)
          )
        ),
        List[Par](GInt(1L))
      )
    val indirectResult = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(nthCallEvalToSend)(env, splitRand)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(indirectResult)(channel, Seq(GString("Success")), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of New" should "use deterministic names and provide urn-based resources" in {
    implicit val errorLog = new ErrorLog()

    val splitRand   = rand.splitByte(42)
    val resultRand  = rand.splitByte(42)
    val chosenName  = resultRand.next
    val result0Rand = resultRand.splitByte(0)
    val result1Rand = resultRand.splitByte(1)
    val newProc: New =
      New(
        bindCount = 2,
        uri = List("rho:test:foo"),
        p = Par(
          sends = List(
            Send(GString("result0"), List(EVar(BoundVar(0))), locallyFree = BitSet(0)),
            Send(GString("result1"), List(EVar(BoundVar(1))), locallyFree = BitSet(1))
          ),
          locallyFree = BitSet(0, 1)
        )
      )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, _) =>
        def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))
        val reducer = RholangOnlyDispatcher
          .create[Task, Task.Par](space, Map("rho:test:foo" -> byteName(42)))
          ._2
        reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(newProc)(env, splitRand)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel0: Par = GString("result0")
    val channel1: Par = GString("result1")
    // format: off
    result should be(
      HashMap(
        List(channel0) ->
          Row(
            List(
              Datum.create(
                channel0,
                ListParWithRandom(Seq(GPrivate(ByteString.copyFrom(Array[Byte](42)))), result0Rand),
                false)),
            List()),
        List(channel1) ->
          Row(
            List(
              Datum.create(
                channel1,
                ListParWithRandom(Seq(GPrivate(ByteString.copyFrom(chosenName))), result1Rand),
                false)),
            List())
      )
    )
  }
  // format: on
  "eval of nth method in send position" should "change what is sent" in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    val nthCallEvalToSend: Expr =
      EMethod(
        "nth",
        EList(
          List(
            GInt(7L),
            Send(GString("result"), List(GString("Success")), false, BitSet()),
            GInt(9L),
            GInt(10L)
          )
        ),
        List[Par](GInt(1L))
      )
    val send: Par =
      Send(GString("result"), List[Par](nthCallEvalToSend), false, BitSet())
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(send)(env, splitRand)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    // format: off
    checkData(result)(channel, Seq(Send(GString("result"), List(GString("Success")), false, BitSet())), splitRand)
    // format: on
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of a method" should "substitute target before evaluating" in {
    implicit val errorLog = new ErrorLog()

    val hexToBytesCall: Expr =
      EMethod("hexToBytes", Expr(EVarBody(EVar(Var(BoundVar(0))))))
    val directResult: Par = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par](Expr(GString("deadbeef")))
        Await.result(reducer.evalExprToPar(hexToBytesCall).runToFuture, 3.seconds)
    }
    val expectedResult: Par = Expr(GByteArray(ByteString.copyFrom(Base16.decode("deadbeef"))))
    directResult should be(expectedResult)

    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of `toByteArray` method on any process" should "return that process serialized" in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    import coop.rchain.models.serialization.implicits._
    val proc = Receive(
      Seq(ReceiveBind(Seq(EVar(FreeVar(0))), GString("channel"))),
      Par(),
      false,
      1,
      BitSet()
    )
    val serializedProcess =
      com.google.protobuf.ByteString.copyFrom(Serialize[Par].encode(proc).toArray)
    val toByteArrayCall           = EMethod("toByteArray", proc, List[Par]())
    def wrapWithSend(p: Par): Par = Send(GString("result"), List[Par](p), false, BitSet())
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(result)(channel, Seq(Expr(GByteArray(serializedProcess))), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "substitute before serialization" in {
    implicit val errorLog = new ErrorLog()

    val splitRand = rand.splitByte(0)
    val unsubProc: Par =
      New(bindCount = 1, p = EVar(BoundVar(1)), locallyFree = BitSet(0))
    val subProc: Par =
      New(bindCount = 1, p = GPrivateBuilder("zero"), locallyFree = BitSet())
    val serializedProcess         = subProc.toByteString
    val toByteArrayCall: Par      = EMethod("toByteArray", unsubProc, List[Par](), BitSet(0))
    val channel: Par              = GString("result")
    def wrapWithSend(p: Par): Par = Send(channel, List[Par](p), false, p.locallyFree)

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env.makeEnv[Par](GPrivateBuilder("one"), GPrivateBuilder("zero"))
        val task        = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    checkData(result)(channel, Seq(Expr(GByteArray(serializedProcess))), splitRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "return an error when `toByteArray` is called with arguments" in {
    implicit val errorLog = new ErrorLog()

    val toByteArrayWithArgumentsCall: EMethod =
      EMethod(
        "toByteArray",
        Par(sends = Seq(Send(GString("result"), List(GString("Success")), false, BitSet()))),
        List[Par](GInt(1L))
      )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toByteArrayWithArgumentsCall)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(MethodArgumentNumberMismatch("toByteArray", 0, 1))
    )
  }

  "eval of hexToBytes" should "transform encoded string to byte array (not the rholang term)" in {
    import coop.rchain.models.serialization.implicits._
    implicit val errorLog = new ErrorLog()

    val splitRand                 = rand.splitByte(0)
    val testString                = "testing testing"
    val base16Repr                = Base16.encode(testString.getBytes)
    val proc: Par                 = GString(base16Repr)
    val toByteArrayCall           = EMethod("hexToBytes", proc, List[Par]())
    def wrapWithSend(p: Par): Par = Send(GString("result"), List[Par](p), false, BitSet())
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(result)(
      channel,
      Seq(Expr(GByteArray(ByteString.copyFrom(testString.getBytes)))),
      splitRand
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "eval of `toUtf8Bytes`" should "transform string to UTF-8 byte array (not the rholang term)" in {
    import coop.rchain.models.serialization.implicits._
    implicit val errorLog         = new ErrorLog()
    val splitRand                 = rand.splitByte(0)
    val testString                = "testing testing"
    val proc: Par                 = GString(testString)
    val toUtf8BytesCall           = EMethod("toUtf8Bytes", proc, List[Par]())
    def wrapWithSend(p: Par): Par = Send(GString("result"), List[Par](p), false, BitSet())
    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(wrapWithSend(toUtf8BytesCall))(env, splitRand)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")

    checkData(result)(
      channel,
      Seq(Expr(GByteArray(ByteString.copyFrom(testString.getBytes)))),
      splitRand
    )
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "return an error when `toUtf8Bytes` is called with arguments" in {
    implicit val errorLog = new ErrorLog()
    val toUtfBytesWithArgumentsCall: EMethod =
      EMethod(
        "toUtf8Bytes",
        Par(sends = Seq(Send(GString("result"), List(GString("Success")), false, BitSet()))),
        List[Par](GInt(1L))
      )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toUtfBytesWithArgumentsCall)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(
      Vector(MethodArgumentNumberMismatch("toUtf8Bytes", 0, 1))
    )
  }

  it should "return an error when `toUtf8Bytes` is evaluated on a non String" in {
    implicit val errorLog = new ErrorLog()
    val toUtfBytesCall    = EMethod("toUtf8Bytes", GInt(44L), List[Par]())

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toUtfBytesCall)
        val inspectTask = for {
          _ <- nthTask
        } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result should be(HashMap.empty)
    errorLog.readAndClearErrorVector should be(Vector(MethodNotDefined("toUtf8Bytes", "Int")))
  }

  "variable references" should "be substituted before being used." in {
    implicit val errorLog = new ErrorLog()
    val splitRandResult   = rand.splitByte(3)
    val splitRandSrc      = rand.splitByte(3)
    splitRandResult.next()
    val mergeRand =
      Blake2b512Random.merge(Seq(splitRandResult.splitByte(1), splitRandResult.splitByte(0)))
    val proc = New(
      bindCount = 1,
      p = Par(
        sends = List(
          Send(
            chan = EVar(BoundVar(0)),
            data = List(EVar(BoundVar(0))),
            persistent = false
          )
        ),
        receives = List(
          Receive(
            binds = List(
              ReceiveBind(
                patterns = List(Connective(VarRefBody(VarRef(0, 1)))),
                source = EVar(BoundVar(0)),
                freeCount = 0
              )
            ),
            body = Send(chan = GString("result"), data = List(GString("true"))),
            bindCount = 0
          )
        )
      )
    )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(proc)(env, splitRandSrc)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")
    checkData(result)(channel, Seq(GString("true")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "be substituted before being used in a match." in {
    implicit val errorLog = new ErrorLog()
    val splitRandResult   = rand.splitByte(4)
    val splitRandSrc      = rand.splitByte(4)
    splitRandResult.next()
    val proc = New(
      bindCount = 1,
      p = Match(
        target = EVar(BoundVar(0)),
        cases = List(
          MatchCase(
            pattern = Connective(VarRefBody(VarRef(0, 1))),
            source = Send(chan = GString("result"), data = List(GString("true")))
          )
        )
      )
    )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(proc)(env, splitRandSrc)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val channel: Par = GString("result")

    checkData(result)(channel, Seq(GString("true")), splitRandResult)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  it should "reference a variable that comes from a match in tuplespace" in {
    implicit val errorLog = new ErrorLog()
    val baseRand          = rand.splitByte(7)
    val splitRand0        = baseRand.splitByte(0)
    val splitRand1        = baseRand.splitByte(1)
    val mergeRand         = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val proc = Par(
      sends = List(Send(chan = GInt(7L), data = List(GInt(10L)))),
      receives = List(
        Receive(
          binds = List(
            ReceiveBind(
              patterns = List(EVar(FreeVar(0))),
              source = GInt(7L),
              freeCount = 1
            )
          ),
          body = Match(
            GInt(10L),
            List(
              MatchCase(
                pattern = Connective(VarRefBody(VarRef(0, 1))),
                source = Send(chan = GString("result"), data = List(GString("true")))
              )
            )
          )
        )
      )
    )

    val result = withTestSpace(errorLog) {
      case TestFixture(space, reducer) =>
        val env         = Env[Par]()
        val task        = reducer.eval(proc)(env, baseRand)
        val inspectTask = for { _ <- task } yield space.store.toMap
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    val channel: Par = GString("result")
    checkData(result)(channel, Seq(GString("true")), mergeRand)
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "1 matches 1" should "return true" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask  = reducer.evalExpr(EMatches(GInt(1L), GInt(1L)))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "1 matches 0" should "return false" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask  = reducer.evalExpr(EMatches(GInt(1L), GInt(0L)))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GBool(false))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "1 matches _" should "return true" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask  = reducer.evalExpr(EMatches(GInt(1L), EVar(Wildcard(Var.WildcardMsg()))))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "x matches 1" should "return true when x is bound to 1" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par](GInt(1L))
        val inspectTask  = reducer.evalExpr(EMatches(EVar(BoundVar(0)), GInt(1L)))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "1 matches =x" should "return true when x is bound to 1" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par](GInt(1L))

        val inspectTask = reducer.evalExpr(EMatches(GInt(1L), Connective(VarRefBody(VarRef(0, 1)))))

        Await.result(inspectTask.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abc'.length()" should "return the length of the string" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask  = reducer.evalExpr(EMethodBody(EMethod("length", GString("abc"))))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abcabac'.slice(3, 6)" should "return 'aba'" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(3L), GInt(6L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("aba"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abcabcac'.slice(2,1)" should "return empty string" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(2L), GInt(1L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString(""))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abcabcac'.slice(8,9)" should "return empty string" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(8L), GInt(9L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString(""))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abcabcac'.slice(-2,2)" should "return 'ab'" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(-2L), GInt(2L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("ab"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'Hello, ${name}!' % {'name': 'Alice'}" should "return 'Hello, Alice!" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EPercentPercentBody(
            EPercentPercent(
              GString("Hello, ${name}!"),
              EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("Alice")))))
            )
          )
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("Hello, Alice!"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "'abc' ++ 'def'" should "return 'abcdef" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              GString("abc"),
              GString("def")
            )
          )
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("abcdef"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "ByteArray('dead') ++ ByteArray('beef)'" should "return ByteArray('deadbeef')" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              GByteArray(ByteString.copyFrom(Base16.decode("dead"))),
              GByteArray(ByteString.copyFrom(Base16.decode("beef")))
            )
          )
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GByteArray(ByteString.copyFrom(Base16.decode("deadbeef"))))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  def interpolate(base: String, substitutes: Seq[(Par, Par)]): Expr =
    EPercentPercentBody(
      EPercentPercent(
        GString(base),
        EMapBody(ParMap(substitutes))
      )
    )

  "'${a} ${b}' % {'a': '1 ${b}', 'b': '2 ${a}'" should "return '1 ${b} 2 ${a}" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val inspectTask = reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            List[(Par, Par)](
              (GString("a"), GString("1 ${b}")),
              (GString("b"), GString("2 ${a}"))
            )
          )
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("1 ${b} 2 ${a}"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "interpolate" should "interpolate Boolean values" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val task = reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            Seq[(Par, Par)](
              (GString("a"), GBool(false)),
              (GString("b"), GBool(true))
            )
          )
        )
        Await.result(task.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GString("false true"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "interpolate" should "interpolate URIs" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val task = reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            Seq[(Par, Par)](
              (GString("a"), GUri("testUriA")),
              (GString("b"), GUri("testUriB"))
            )
          )
        )
        Await.result(task.runToFuture, 3.seconds)
    }

    result.exprs should be(Seq(Expr(GString("testUriA testUriB"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[0, 1, 2, 3].length()" should "return the length of the list" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(0L), GInt(1L), GInt(2L), GInt(3L)))
        val inspectTask  = reducer.evalExpr(EMethodBody(EMethod("length", list)))

        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GInt(4L))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(3, 5)" should "return [9, 4]" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(3L), GInt(5L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List(GInt(9L), GInt(4L)))))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(5, 4)" should "return []" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(5L), GInt(4L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List())))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(7, 8)" should "return []" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(7L), GInt(8L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List())))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(-2, 2)" should "return [3, 7]" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(-2L), GInt(2L))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List(GInt(3L), GInt(7L)))))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "[3, 2, 9] ++ [6, 1, 7]" should "return [3, 2, 9, 6, 1, 7]" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsList      = EList(List(GInt(3L), GInt(2L), GInt(9L)))
        val rhsList      = EList(List(GInt(6L), GInt(1L), GInt(7L)))
        val inspectTask = reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              lhsList,
              rhsList
            )
          )
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultList = EList(List(GInt(3L), GInt(2L), GInt(9L), GInt(6L), GInt(1L), GInt(7L)))
    result.exprs should be(Seq(Expr(EListBody(resultList))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b'}.getOrElse(1, 'c')" should "return 'a'" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("getOrElse", map, List(GInt(1L), GString("c"))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("a"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b'}.getOrElse(3, 'c')" should "return 'c'" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("getOrElse", map, List(GInt(3L), GString("c"))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GString("c"))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b'}.set(3, 'c')" should "return {1: 'a', 2: 'b', 3: 'c'}" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("set", map, List(GInt(3L), GString("c"))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultMap = EMapBody(
      ParMap(
        List[(Par, Par)](
          (GInt(1L), GString("a")),
          (GInt(2L), GString("b")),
          (GInt(3L), GString("c"))
        )
      )
    )
    result.exprs should be(Seq(Expr(resultMap)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b'}.set(2, 'c')" should "return {1: 'a', 2: 'c'}" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("set", map, List(GInt(2L), GString("c"))))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultMap =
      EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("c")))))
    result.exprs should be(Seq(Expr(resultMap)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b', 3: 'c'}.keys()" should "return Set(1, 2, 3)" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map = EMapBody(
          ParMap(
            List[(Par, Par)](
              (GInt(1L), GString("a")),
              (GInt(2L), GString("b")),
              (GInt(3L), GString("c"))
            )
          )
        )
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("keys", map))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultSet = ESetBody(
      ParSet(
        List[Par](GInt(1L), GInt(2L), GInt(3L))
      )
    )
    result.exprs should be(Seq(Expr(resultSet)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b', 3: 'c'}.size()" should "return 3" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map = EMapBody(
          ParMap(
            List[(Par, Par)](
              (GInt(1L), GString("a")),
              (GInt(2L), GString("b")),
              (GInt(3L), GString("c"))
            )
          )
        )
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("size", map))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2, 3).size()" should "return 3" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()

        val set = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        val inspectTask = reducer.evalExpr(
          EMethodBody(EMethod("size", set))
        )

        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2) + 3" should "return Set(1, 2, 3)" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        val inspectTask = reducer.evalExpr(
          EPlusBody(EPlus(set, GInt(3L)))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
    result.exprs should be(Seq(Expr(resultSet)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b', 3: 'c'} - 3" should "return {1: 'a', 2: 'b'}" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map = EMapBody(
          ParMap(
            List[(Par, Par)](
              (GInt(1L), GString("a")),
              (GInt(2L), GString("b")),
              (GInt(3L), GString("c"))
            )
          )
        )
        val inspectTask = reducer.evalExpr(
          EMinusBody(EMinus(map, GInt(3L)))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultMap =
      EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
    result.exprs should be(Seq(Expr(resultMap)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2, 3) - 3" should "return Set(1, 2)" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        val inspectTask = reducer.evalExpr(
          EMinusBody(EMinus(set, GInt(3L)))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
    result.exprs should be(Seq(Expr(resultSet)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2) ++ Set(3, 4)" should "return Set(1, 2, 3, 4)" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        val rhsSet       = ESetBody(ParSet(List[Par](GInt(3L), GInt(4L))))
        val inspectTask = reducer.evalExpr(
          EPlusPlusBody(EPlusPlus(lhsSet, rhsSet))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L), GInt(4L))))
    result.exprs should be(Seq(Expr(resultSet)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "{1: 'a', 2: 'b'} ++ {3: 'c', 4: 'd'}" should "return union" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsMap =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val rhsMap =
          EMapBody(ParMap(List[(Par, Par)]((GInt(3L), GString("c")), (GInt(4L), GString("d")))))
        val inspectTask = reducer.evalExpr(
          EPlusPlusBody(EPlusPlus(lhsMap, rhsMap))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultMap = EMapBody(
      ParMap(
        List[(Par, Par)](
          (GInt(1L), GString("a")),
          (GInt(2L), GString("b")),
          (GInt(3L), GString("c")),
          (GInt(4L), GString("d"))
        )
      )
    )
    result.exprs should be(Seq(Expr(resultMap)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2, 3, 4) -- Set(1, 2)" should "return Set(3, 4)" in {
    implicit val errorLog = new ErrorLog()

    val result = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L), GInt(4L))))
        val rhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        val inspectTask = reducer.evalExpr(
          EMinusMinusBody(EMinusMinus(lhsSet, rhsSet))
        )
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(3L), GInt(4L))))
    result.exprs should be(Seq(Expr(resultSet)))
    errorLog.readAndClearErrorVector should be(Vector.empty[InterpreterError])
  }

  "Set(1, 2, 3).get(1)" should "not work" in {
    implicit val errorLog = new ErrorLog()

    withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        val inspectTask  = reducer.eval(EMethodBody(EMethod("get", set, List(GInt(1L)))))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    errorLog.readAndClearErrorVector should be(
      Vector(MethodNotDefined("get", "Set"))
    )
  }

  "{1: 'a', 2: 'b'}.add(1)" should "not work" in {
    implicit val errorLog = new ErrorLog()

    withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val inspectTask = reducer.eval(EMethodBody(EMethod("add", map, List(GInt(1L)))))
        Await.result(inspectTask.runToFuture, 3.seconds)
    }
    errorLog.readAndClearErrorVector should be(
      Vector(MethodNotDefined("add", "Map"))
    )
  }

  "Running out of phlogistons" should "stop the evaluation" in {
    implicit val errorLog = new ErrorLog()

    val test = withTestSpace(errorLog) {
      case TestFixture(_, reducer) =>
        implicit val env   = Env.makeEnv[Par]()
        val notEnoughPhlos = Cost(5)
        reducer.setAvailablePhlos(notEnoughPhlos).runSyncUnsafe(1.second)
        val splitRand = rand.splitByte(0)
        val receive =
          Receive(
            Seq(
              ReceiveBind(
                Seq(Par(exprs = Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))))),
                Par(exprs = Seq(GString("channel")))
              )
            ),
            Par(),
            false,
            3,
            BitSet()
          )
        reducer.eval(receive)(env, splitRand)
    }

    val result = test.attempt.runSyncUnsafe(1.second)
    assert(result === Left(OutOfPhlogistonsError))
    errorLog.readAndClearErrorVector() should be(Vector.empty)
  }
}
