package coop.rchain.rholang.interpreter

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.NoopSpan
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.error_handling.{_error, ErrorHandling}
import coop.rchain.rholang.interpreter.error_handling.errors._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{AppendedClues, Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.collection.mutable.HashMap
import scala.collection.{mutable, SortedSet}
import scala.concurrent.duration._

class ReduceSpec extends FlatSpec with Matchers with AppendedClues with PersistentStoreTester {
  implicit val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])

  private[this] def mapData(elements: Map[Par, (Seq[Par], Blake2b512Random)]): Iterable[
    (
        Seq[Par],
        Row[BindPattern, ListParWithRandom, TaggedContinuation]
    )
  ] =
    elements.map {
      case (channel, (data, rand)) =>
        Seq(channel) ->
          Row[BindPattern, ListParWithRandom, TaggedContinuation](
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
            List.empty
          )
    }.toIterable

  private[this] def checkContinuation(
      result: Map[
        Seq[Par],
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
                persist = false,
                SortedSet.empty
              )
            )
          )
      )
    )

  "evalExpr" should "handle simple addition" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val result = fixture {
      case (_, reducer) =>
        val addExpr      = EPlus(GInt(7L), GInt(8L))
        implicit val env = Env[Par]()
        reducer.evalExpr(addExpr)
    }

    val expected = Seq(Expr(GInt(15L)))
    result.exprs should be(expected)
    error.get.unsafeRunSync should be(None)
  }

  "evalExpr" should "handle long addition" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val result = fixture {
      case (_, reducer) =>
        val addExpr      = EPlus(GInt(Int.MaxValue), GInt(Int.MaxValue))
        implicit val env = Env[Par]()
        reducer.evalExpr(addExpr)
    }

    val expected = Seq(Expr(GInt(2 * Int.MaxValue.toLong)))
    result.exprs should be(expected)
    error.get.unsafeRunSync should be(None)
  }

  "evalExpr" should "leave ground values alone" in {
    implicit val error: _error[Task] = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val result = fixture {
      case (_, reducer) =>
        val groundExpr   = GInt(7L)
        implicit val env = Env[Par]()
        reducer.evalExpr(groundExpr)
    }

    val expected = Seq(Expr(GInt(7L)))
    result.exprs should be(expected)
    error.get.unsafeRunSync should be(None)
  }

  "evalExpr" should "handle equality between arbitary processes" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val result = fixture {
      case (_, reducer) =>
        val eqExpr       = EEq(GPrivateBuilder("private_name"), GPrivateBuilder("private_name"))
        implicit val env = Env[Par]()
        reducer.evalExpr(eqExpr)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    error.get.unsafeRunSync should be(None)
  }

  "evalExpr" should "substitute before comparison." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val result = fixture {
      case (_, reducer) =>
        implicit val emptyEnv = Env.makeEnv(Par(), Par())
        val eqExpr            = EEq(EVar(BoundVar(0)), EVar(BoundVar(1)))
        reducer.evalExpr(eqExpr)
    }
    val expected = Seq(Expr(GBool(true)))
    result.exprs should be(expected)
    error.get.unsafeRunSync should be(None)
  }

  "eval of Bundle" should "evaluate contents of bundle" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val splitRand      = rand.splitByte(0)
    val channel: Par   = GString("channel")
    val result = fixture {
      case (space, reducer) =>
        val bundleSend =
          Bundle(Send(channel, List(GInt(7L), GInt(8L), GInt(9L)), persistent = false, BitSet()))
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(bundleSend)(env, splitRand)
        for {
          _   <- resultTask
          res <- space.toMap
        } yield res
    }

    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GInt(7L), GInt(8L), GInt(9L)), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "throw an error if names are used against their polarity" in {
    implicit val error: _error[Task] = ErrorHandling.emptyError[Task].runSyncUnsafe()

    /* for (n <- @bundle+ { y } ) { }  -> for (n <- y) { }
     */
    val y = GString("y")
    val receive = Receive(
      Seq(ReceiveBind(Seq(Par()), Bundle(y, writeFlag = true))),
      Par()
    )

    val receiveResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        reducer.eval(receive) >> space.toMap
    }
    receiveResult should be(HashMap.empty)
    error.getAndSet(None).unsafeRunSync should be(
      Some(ReduceError("Trying to read from non-readable channel."))
    )

    /* @bundle- { x } !(7) -> x!(7)
     */
    val x = GString("channel")
    val send =
      Send(Bundle(x, writeFlag = false, readFlag = true), Seq(Expr(GInt(7L))))

    val sendResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        reducer.eval(send) >> space.toMap
    }
    sendResult should be(HashMap.empty)
    error.get.unsafeRunSync should be(
      Some(ReduceError("Trying to send on non-writeable channel."))
    )
  }

  "eval of Send" should "place something in the tuplespace." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val channel: Par   = GString("channel")
    val splitRand      = rand.splitByte(0)
    val result = fixture {
      case (space, reducer) =>
        val send =
          Send(channel, List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(send)(env, splitRand)
        for {
          _   <- resultTask
          res <- space.toMap
        } yield res
    }

    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GInt(7L), GInt(8L), GInt(9L)), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "verify that Bundle is writeable before sending on Bundle " in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    /* @bundle+ { x } !(7) -> x!(7)
     */
    val channel: Par = GString("channel")
    val send =
      Send(Bundle(channel, writeFlag = true), Seq(Expr(GInt(7L))))

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        reducer.eval(send)(env, splitRand).flatMap(_ => space.toMap)
    }

    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GInt(7L)), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of single channel Receive" should "place something in the tuplespace." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand    = rand.splitByte(0)
    val channel: Par = GString("channel")
    val result = fixture {
      case (space, reducer) =>
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
            false,
            3,
            BitSet()
          )
        implicit val env = Env[Par]()
        val resultTask   = reducer.eval(receive)(env, splitRand)
        for {
          _   <- resultTask
          res <- space.toMap
        } yield res
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
    error.get.unsafeRunSync should be(None)
  }

  it should "verify that bundle is readable if receiving on Bundle" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(1)
    /* for (@Nil <- @bundle- { y } ) { }  -> for (n <- y) { }
     */

    val y = GString("y")
    val receive = Receive(
      binds = Seq(
        ReceiveBind(
          patterns = Seq(Par()),
          source = Bundle(y, readFlag = true)
        )
      ),
      body = Par()
    )

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        reducer.eval(receive)(env, splitRand).flatMap(_ => space.toMap)
    }

    val channels = List[Par](y)
    checkContinuation(result)(
      channels,
      List(BindPattern(List(Par()), None)),
      ParWithRandom(Par(), splitRand)
    )
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send | Receive" should "meet in the tuplespace and proceed." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(GString("channel"), List(GInt(7L), GInt(8L), GInt(9L)), persistent = false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GString("channel"),
          freeCount = 3
        )
      ),
      Send(GString("result"), List(GString("Success")), persistent = false, BitSet()),
      persistent = false,
      peek = false,
      3,
      BitSet()
    )
    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")

    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("Success")), mergeRand))
      )
    )

    sendFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }

    receiveFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send | Receive with peek" should "meet in the tuplespace and proceed." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val channel: Par       = GString("channel")
    val resultChannel: Par = GString("result")

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(channel, List(GInt(7L), GInt(8L), GInt(9L)), false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          channel,
          freeCount = 3
        )
      ),
      Send(resultChannel, List(GString("Success")), false, BitSet()),
      false,
      true,
      3,
      BitSet()
    )

    val expectedResult = mapData(
      Map(
        channel       -> ((Seq(GInt(7L), GInt(8L), GInt(9L)), splitRand0)),
        resultChannel -> ((Seq(GString("Success")), mergeRand))
      )
    )

    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    sendFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }

    receiveFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send | Receive" should "when whole list is bound to list remainder, meet in the tuplespace and proceed. (RHOL-422)" in {
    // for(@[...a] <- @"channel") { … } | @"channel"!([7,8,9])
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    // format: off
    val send =
      Send(GString("channel"), List(Par(exprs = Seq(Expr(EListBody(EList(Seq(GInt(7L), GInt(8L), GInt(9L)))))))), persistent = false, BitSet())
    val receive = Receive(
      Seq(
        ReceiveBind(Seq(Par(exprs = Seq(EListBody(EList(connectiveUsed = true, remainder = Some(FreeVar(0))))))),
          GString("channel"),
          freeCount = 1)),
      Send(GString("result"), List(GString("Success")), persistent = false, BitSet()),
      persistent = false,
      peek=false,
      1,
      BitSet()
    )
    // format: on
    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("Success")), mergeRand))
      )
    )
    sendFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }

    receiveFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send on (7 + 8) | Receive on 15" should "meet in the tuplespace and proceed." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(
        EPlus(GInt(7L), GInt(8L)),
        List(GInt(7L), GInt(8L), GInt(9L)),
        persistent = false,
        BitSet()
      )
    val receive = Receive(
      Seq(
        ReceiveBind(
          Seq(EVar(FreeVar(0)), EVar(FreeVar(1)), EVar(FreeVar(2))),
          GInt(15L),
          freeCount = 3
        )
      ),
      Send(GString("result"), List(GString("Success")), persistent = false, BitSet()),
      persistent = false,
      peek = false,
      3,
      BitSet()
    )

    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("Success")), mergeRand))
      )
    )
    sendFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }
    receiveFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send of Receive | Receive" should "meet in the tuplespace and proceed." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val baseRand   = rand.splitByte(2)
    val splitRand0 = baseRand.splitByte(0)
    val splitRand1 = baseRand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val simpleReceive = Receive(
      Seq(ReceiveBind(Seq(GInt(2L)), GInt(2L))),
      Par(),
      persistent = false,
      peek = false,
      0,
      BitSet()
    )
    val send =
      Send(GInt(1L), Seq[Par](simpleReceive), persistent = false, BitSet())
    val receive = Receive(
      Seq(ReceiveBind(Seq(EVar(FreeVar(0))), GInt(1L), freeCount = 1)),
      EVar(BoundVar(0)),
      persistent = false,
      peek = false,
      1,
      BitSet()
    )

    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    val channels = List[Par](GInt(2L))

    // Because they are evaluated separately, nothing is split.
    checkContinuation(sendFirstResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }

    checkContinuation(receiveFirstResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    error.get.unsafeRunSync should be(None)

    val bothResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(Par(receives = Seq(receive), sends = Seq(send)))(env, baseRand)
          res <- space.toMap
        } yield res
    }

    checkContinuation(bothResult)(
      channels,
      List(BindPattern(List(GInt(2L)))),
      ParWithRandom(Par(), mergeRand)
    )
    error.get.unsafeRunSync should be(None)
  }

  "Simple match" should "capture and add to the environment." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    val result = fixture {
      case (space, reducer) =>
        val pattern =
          Send(EVar(FreeVar(0)), List(GInt(7L), EVar(FreeVar(1))), persistent = false, BitSet())
            .withConnectiveUsed(true)
        val sendTarget =
          Send(
            EVar(BoundVar(1)),
            List(GInt(7L), EVar(BoundVar(0))),
            persistent = false,
            BitSet(0, 1)
          )
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
        for {
          _   <- matchTask
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GPrivateBuilder("one"), GPrivateBuilder("zero")), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send | Send | Receive join" should "meet in the tuplespace and proceed." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

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
      false,
      3,
      BitSet()
    )
    val sendFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send1)(env, splitRand0)
          _   <- reducer.eval(send2)(env, splitRand1)
          _   <- reducer.eval(receive)(env, splitRand2)
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("Success")), mergeRand))
      )
    )
    sendFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val receiveFirstResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand2)
          _   <- reducer.eval(send1)(env, splitRand0)
          _   <- reducer.eval(send2)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    receiveFirstResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)

    val interleavedResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(send1)(env, splitRand0)
          _   <- reducer.eval(receive)(env, splitRand2)
          _   <- reducer.eval(send2)(env, splitRand1)
          res <- space.toMap
        } yield res
    }

    interleavedResult.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of Send with remainder receive" should "capture the remainder." in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand0 = rand.splitByte(0)
    val splitRand1 = rand.splitByte(1)
    val mergeRand  = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
    val send =
      Send(GString("channel"), List(GInt(7L), GInt(8L), GInt(9L)), persistent = false, BitSet())
    val receive =
      Receive(
        Seq(ReceiveBind(Seq(), GString("channel"), Some(FreeVar(0)), freeCount = 1)),
        Send(GString("result"), Seq(EVar(BoundVar(0))))
      )

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        for {
          _   <- reducer.eval(receive)(env, splitRand1)
          _   <- reducer.eval(send)(env, splitRand0)
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(EList(List(GInt(7L), GInt(8L), GInt(9L)))), mergeRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of nth method" should "pick out the nth item from a list" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    val nthCall: Expr =
      EMethod("nth", EList(List(GInt(7L), GInt(8L), GInt(9L), GInt(10L))), List[Par](GInt(2L)))
    val directResult: Par = fixture {
      case (_, reducer) =>
        implicit val env = Env[Par]()
        reducer.evalExprToPar(nthCall)
    }
    val expectedResult: Par = GInt(9L)
    directResult should be(expectedResult)

    val nthCallEvalToSend: Expr =
      EMethod(
        "nth",
        EList(
          List(
            GInt(7L),
            Send(GString("result"), List(GString("Success")), persistent = false, BitSet()),
            GInt(9L),
            GInt(10L)
          )
        ),
        List[Par](GInt(1L))
      )
    val indirectResult = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(nthCallEvalToSend)(env, splitRand)
        for {
          _   <- nthTask
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")
    val expectedIndirectResult = mapData(
      Map(
        channel -> ((Seq(GString("Success")), splitRand))
      )
    )
    indirectResult.toIterable should contain theSameElementsAs expectedIndirectResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of nth method" should "pick out the nth item from a ByteArray" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val nthCall: Expr =
      EMethod("nth", GByteArray(ByteString.copyFrom(Array[Byte](1, 2, -1))), List[Par](GInt(2L)))
    val directResult: Par = fixture {
      case (_, reducer) =>
        implicit val env = Env[Par]()
        reducer.evalExprToPar(nthCall)
    }
    val expectedResult: Par = GInt(255.toLong)
    directResult should be(expectedResult)
  }

  "eval of length method" should "get length of ByteArray" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val nthCall: Expr =
      EMethod("length", GByteArray(ByteString.copyFrom(Array[Byte](1, 2, -1))), List[Par]())
    val directResult: Par = fixture {
      case (_, reducer) =>
        implicit val env = Env[Par]()
        reducer.evalExprToPar(nthCall)
    }
    val expectedResult: Par = GInt(3.toLong)
    directResult should be(expectedResult)
  }

  "eval of New" should "use deterministic names and provide urn-based resources" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    implicit val span  = NoopSpan[Task]

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

    val result = fixture {
      case (space, _) =>
        implicit val cost          = CostAccounting.emptyCost[Task].unsafeRunSync
        def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))
        val reducer = RholangOnlyDispatcher
          .create[Task, Task.Par](space, Map("rho:test:foo" -> byteName(42)))
          ._2
        reducer.setPhlo(Cost.UNSAFE_MAX).runSyncUnsafe(1.second)
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(newProc)(env, splitRand)
        for {
          _   <- nthTask
          res <- space.toMap
        } yield res
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
                persist = false)),
            List()),
        List(channel1) ->
          Row(
            List(
              Datum.create(
                channel1,
                ListParWithRandom(Seq(GPrivate(ByteString.copyFrom(chosenName))), result1Rand),
                persist = false)),
            List())
      )
    )
  }
  // format: on
  "eval of nth method in send position" should "change what is sent" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    val nthCallEvalToSend: Expr =
      EMethod(
        "nth",
        EList(
          List(
            GInt(7L),
            Send(GString("result"), List(GString("Success")), persistent = false, BitSet()),
            GInt(9L),
            GInt(10L)
          )
        ),
        List[Par](GInt(1L))
      )
    val send: Par =
      Send(GString("result"), List[Par](nthCallEvalToSend), persistent = false, BitSet())
    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(send)(env, splitRand)
        for {
          _   <- nthTask
          res <- space.toMap
        } yield res
    }

    val channel: Par = GString("result")

    val expectedResult = mapData(
      Map(
        channel -> (
          (
            Seq(Send(GString("result"), List(GString("Success")), persistent = false, BitSet())),
            splitRand
          )
        )
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of a method" should "substitute target before evaluating" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val hexToBytesCall: Expr =
      EMethod("hexToBytes", Expr(EVarBody(EVar(Var(BoundVar(0))))))
    val directResult: Par = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par](Expr(GString("deadbeef")))
        reducer.evalExprToPar(hexToBytesCall)
    }
    val expectedResult: Par = Expr(GByteArray(ByteString.copyFrom(Base16.unsafeDecode("deadbeef"))))
    directResult should be(expectedResult)

    error.get.unsafeRunSync should be(None)
  }

  "eval of `toByteArray` method on any process" should "return that process serialized" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    val proc = Receive(
      Seq(ReceiveBind(Seq(EVar(FreeVar(0))), GString("channel"))),
      Par(),
      persistent = false,
      peek = false,
      1,
      BitSet()
    )
    val serializedProcess =
      com.google.protobuf.ByteString.copyFrom(Serialize[Par].encode(proc).toArray)
    val toByteArrayCall = EMethod("toByteArray", proc, List[Par]())
    def wrapWithSend(p: Par): Par =
      Send(GString("result"), List[Par](p), persistent = false, BitSet())
    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        task >> space.toMap
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(Expr(GByteArray(serializedProcess))), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "substitute before serialization" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand = rand.splitByte(0)
    val unsubProc: Par =
      New(bindCount = 1, p = EVar(BoundVar(1)), locallyFree = BitSet(0))
    val subProc: Par =
      New(bindCount = 1, p = GPrivateBuilder("zero"), locallyFree = BitSet())
    val serializedProcess         = subProc.toByteString
    val toByteArrayCall: Par      = EMethod("toByteArray", unsubProc, List[Par](), BitSet(0))
    val channel: Par              = GString("result")
    def wrapWithSend(p: Par): Par = Send(channel, List[Par](p), persistent = false, p.locallyFree)

    val result = fixture {
      case (space, reducer) =>
        val env  = Env.makeEnv[Par](GPrivateBuilder("one"), GPrivateBuilder("zero"))
        val task = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        task >> space.toMap
    }

    val expectedResult = mapData(
      Map(
        channel -> ((Seq(Expr(GByteArray(serializedProcess))), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "return an error when `toByteArray` is called with arguments" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val toByteArrayWithArgumentsCall: EMethod =
      EMethod(
        "toByteArray",
        Par(
          sends =
            Seq(Send(GString("result"), List(GString("Success")), persistent = false, BitSet()))
        ),
        List[Par](GInt(1L))
      )

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toByteArrayWithArgumentsCall)
        nthTask >> space.toMap
    }
    result should be(HashMap.empty)
    error.get.unsafeRunSync should be(
      Some(MethodArgumentNumberMismatch("toByteArray", 0, 1))
    )
  }

  "eval of hexToBytes" should "transform encoded string to byte array (not the rholang term)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val splitRand       = rand.splitByte(0)
    val testString      = "testing testing"
    val base16Repr      = Base16.encode(testString.getBytes)
    val proc: Par       = GString(base16Repr)
    val toByteArrayCall = EMethod("hexToBytes", proc, List[Par]())
    def wrapWithSend(p: Par): Par =
      Send(GString("result"), List[Par](p), persistent = false, BitSet())
    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(wrapWithSend(toByteArrayCall))(env, splitRand)
        task >> space.toMap
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(Expr(GByteArray(ByteString.copyFrom(testString.getBytes)))), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "eval of `toUtf8Bytes`" should "transform string to UTF-8 byte array (not the rholang term)" in {
    implicit val error  = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val splitRand       = rand.splitByte(0)
    val testString      = "testing testing"
    val proc: Par       = GString(testString)
    val toUtf8BytesCall = EMethod("toUtf8Bytes", proc, List[Par]())
    def wrapWithSend(p: Par): Par =
      Send(GString("result"), List[Par](p), persistent = false, BitSet())
    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(wrapWithSend(toUtf8BytesCall))(env, splitRand)
        task >> space.toMap
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(Expr(GByteArray(ByteString.copyFrom(testString.getBytes)))), splitRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "return an error when `toUtf8Bytes` is called with arguments" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toUtfBytesWithArgumentsCall: EMethod =
      EMethod(
        "toUtf8Bytes",
        Par(
          sends =
            Seq(Send(GString("result"), List(GString("Success")), persistent = false, BitSet()))
        ),
        List[Par](GInt(1L))
      )

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toUtfBytesWithArgumentsCall)
        nthTask >> space.toMap
    }
    result should be(HashMap.empty)
    error.get.unsafeRunSync should be(
      Some(MethodArgumentNumberMismatch("toUtf8Bytes", 0, 1))
    )
  }

  it should "return an error when `toUtf8Bytes` is evaluated on a non String" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toUtfBytesCall = EMethod("toUtf8Bytes", GInt(44L), List[Par]())

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toUtfBytesCall)
        nthTask >> space.toMap
    }
    result should be(HashMap.empty)
    error.get.unsafeRunSync should be(
      Some(MethodNotDefined("toUtf8Bytes", "Int"))
    )
  }

  "variable references" should "be substituted before being used." in {
    implicit val error  = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val splitRandResult = rand.splitByte(3)
    val splitRandSrc    = rand.splitByte(3)
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

    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(proc)(env, splitRandSrc)
        task >> space.toMap
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("true")), mergeRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "be substituted before being used in a match." in {
    implicit val error  = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val splitRandResult = rand.splitByte(4)
    val splitRandSrc    = rand.splitByte(4)
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

    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(proc)(env, splitRandSrc)
        task >> space.toMap
    }
    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("true")), splitRandResult))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  it should "reference a variable that comes from a match in tuplespace" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val baseRand       = rand.splitByte(7)
    val splitRand0     = baseRand.splitByte(0)
    val splitRand1     = baseRand.splitByte(1)
    val mergeRand      = Blake2b512Random.merge(Seq(splitRand1, splitRand0))
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

    val result = fixture {
      case (space, reducer) =>
        val env  = Env[Par]()
        val task = reducer.eval(proc)(env, baseRand)
        task >> space.toMap
    }

    val channel: Par = GString("result")
    val expectedResult = mapData(
      Map(
        channel -> ((Seq(GString("true")), mergeRand))
      )
    )
    result.toIterable should contain theSameElementsAs expectedResult
    error.get.unsafeRunSync should be(None)
  }

  "1 matches 1" should "return true" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(EMatches(GInt(1L), GInt(1L)))
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    error.get.unsafeRunSync should be(None)
  }

  "1 matches 0" should "return false" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(EMatches(GInt(1L), GInt(0L)))
    }

    result.exprs should be(Seq(Expr(GBool(false))))
    error.get.unsafeRunSync should be(None)
  }

  "1 matches _" should "return true" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(EMatches(GInt(1L), EVar(Wildcard(Var.WildcardMsg()))))
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    error.get.unsafeRunSync should be(None)
  }

  "x matches 1" should "return true when x is bound to 1" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par](GInt(1L))
        reducer.evalExpr(EMatches(EVar(BoundVar(0)), GInt(1L)))
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    error.get.unsafeRunSync should be(None)
  }

  "1 matches =x" should "return true when x is bound to 1" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par](GInt(1L))
        reducer.evalExpr(EMatches(GInt(1L), Connective(VarRefBody(VarRef(0, 1)))))
    }

    result.exprs should be(Seq(Expr(GBool(true))))
    error.get.unsafeRunSync should be(None)
  }

  "'abc'.length()" should "return the length of the string" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(EMethodBody(EMethod("length", GString("abc"))))
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    error.get.unsafeRunSync should be(None)
  }

  "'abcabac'.slice(3, 6)" should "return 'aba'" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(3L), GInt(6L))))
        )
    }
    result.exprs should be(Seq(Expr(GString("aba"))))
    error.get.unsafeRunSync should be(None)
  }

  "'abcabcac'.slice(2,1)" should "return empty string" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(2L), GInt(1L))))
        )
    }
    result.exprs should be(Seq(Expr(GString(""))))
    error.get.unsafeRunSync should be(None)
  }

  "'abcabcac'.slice(8,9)" should "return empty string" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(8L), GInt(9L))))
        )
    }
    result.exprs should be(Seq(Expr(GString(""))))
    error.get.unsafeRunSync should be(None)
  }

  "'abcabcac'.slice(-2,2)" should "return 'ab'" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EMethodBody(EMethod("slice", GString("abcabac"), List(GInt(-2L), GInt(2L))))
        )
    }
    result.exprs should be(Seq(Expr(GString("ab"))))
    error.get.unsafeRunSync should be(None)
  }

  "'Hello, ${name}!' % {'name': 'Alice'}" should "return 'Hello, Alice!" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EPercentPercentBody(
            EPercentPercent(
              GString("Hello, ${name}!"),
              EMapBody(ParMap(List[(Par, Par)]((GString("name"), GString("Alice")))))
            )
          )
        )
    }
    result.exprs should be(Seq(Expr(GString("Hello, Alice!"))))
    error.get.unsafeRunSync should be(None)
  }

  "'abc' ++ 'def'" should "return 'abcdef" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              GString("abc"),
              GString("def")
            )
          )
        )
    }
    result.exprs should be(Seq(Expr(GString("abcdef"))))
    error.get.unsafeRunSync should be(None)
  }

  "ByteArray('dead') ++ ByteArray('beef)'" should "return ByteArray('deadbeef')" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              GByteArray(ByteString.copyFrom(Base16.unsafeDecode("dead"))),
              GByteArray(ByteString.copyFrom(Base16.unsafeDecode("beef")))
            )
          )
        )
    }
    result.exprs should be(
      Seq(Expr(GByteArray(ByteString.copyFrom(Base16.unsafeDecode("deadbeef")))))
    )
    error.get.unsafeRunSync should be(None)
  }

  def interpolate(base: String, substitutes: Seq[(Par, Par)]): Expr =
    EPercentPercentBody(
      EPercentPercent(
        GString(base),
        EMapBody(ParMap(substitutes))
      )
    )

  "'${a} ${b}' % {'a': '1 ${b}', 'b': '2 ${a}'" should "return '1 ${b} 2 ${a}" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            List[(Par, Par)](
              (GString("a"), GString("1 ${b}")),
              (GString("b"), GString("2 ${a}"))
            )
          )
        )
    }
    result.exprs should be(Seq(Expr(GString("1 ${b} 2 ${a}"))))
    error.get.unsafeRunSync should be(None)
  }

  "interpolate" should "interpolate Boolean values" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            Seq[(Par, Par)](
              (GString("a"), GBool(false)),
              (GString("b"), GBool(true))
            )
          )
        )
    }

    result.exprs should be(Seq(Expr(GString("false true"))))
    error.get.unsafeRunSync should be(None)
  }

  "interpolate" should "interpolate URIs" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        reducer.evalExpr(
          interpolate(
            "${a} ${b}",
            Seq[(Par, Par)](
              (GString("a"), GUri("testUriA")),
              (GString("b"), GUri("testUriB"))
            )
          )
        )
    }

    result.exprs should be(Seq(Expr(GString("testUriA testUriB"))))
    error.get.unsafeRunSync should be(None)
  }

  "[0, 1, 2, 3].length()" should "return the length of the list" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(0L), GInt(1L), GInt(2L), GInt(3L)))
        reducer.evalExpr(EMethodBody(EMethod("length", list)))
    }
    result.exprs should be(Seq(Expr(GInt(4L))))
    error.get.unsafeRunSync should be(None)
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(3, 5)" should "return [9, 4]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(3L), GInt(5L))))
        )
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List(GInt(9L), GInt(4L)))))))
    error.get.unsafeRunSync should be(None)
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(5, 4)" should "return []" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(5L), GInt(4L))))
        )
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List())))))
    error.get.unsafeRunSync should be(None)
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(7, 8)" should "return []" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(7L), GInt(8L))))
        )
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List())))))
    error.get.unsafeRunSync should be(None)
  }

  "[3, 7, 2, 9, 4, 3, 7].slice(-2, 2)" should "return [3, 7]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val list         = EList(List(GInt(3L), GInt(7L), GInt(2L), GInt(9L), GInt(4L), GInt(3L), GInt(7L)))
        reducer.evalExpr(
          EMethodBody(EMethod("slice", list, List(GInt(-2L), GInt(2L))))
        )
    }
    result.exprs should be(Seq(Expr(EListBody(EList(List(GInt(3L), GInt(7L)))))))
    error.get.unsafeRunSync should be(None)
  }

  "[3, 2, 9] ++ [6, 1, 7]" should "return [3, 2, 9, 6, 1, 7]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsList      = EList(List(GInt(3L), GInt(2L), GInt(9L)))
        val rhsList      = EList(List(GInt(6L), GInt(1L), GInt(7L)))
        reducer.evalExpr(
          EPlusPlusBody(
            EPlusPlus(
              lhsList,
              rhsList
            )
          )
        )
    }
    val resultList = EList(List(GInt(3L), GInt(2L), GInt(9L), GInt(6L), GInt(1L), GInt(7L)))
    result.exprs should be(Seq(Expr(EListBody(resultList))))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b'}.getOrElse(1, 'c')" should "return 'a'" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        reducer.evalExpr(
          EMethodBody(EMethod("getOrElse", map, List(GInt(1L), GString("c"))))
        )
    }
    result.exprs should be(Seq(Expr(GString("a"))))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b'}.getOrElse(3, 'c')" should "return 'c'" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        reducer.evalExpr(
          EMethodBody(EMethod("getOrElse", map, List(GInt(3L), GString("c"))))
        )
    }
    result.exprs should be(Seq(Expr(GString("c"))))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b'}.set(3, 'c')" should "return {1: 'a', 2: 'b', 3: 'c'}" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        reducer.evalExpr(EMethodBody(EMethod("set", map, List(GInt(3L), GString("c")))))
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
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b'}.set(2, 'c')" should "return {1: 'a', 2: 'c'}" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        reducer.evalExpr(EMethodBody(EMethod("set", map, List(GInt(2L), GString("c")))))
    }
    val resultMap =
      EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("c")))))
    result.exprs should be(Seq(Expr(resultMap)))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b', 3: 'c'}.keys()" should "return Set(1, 2, 3)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
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
        reducer.evalExpr(EMethodBody(EMethod("keys", map)))
    }
    val resultSet = ESetBody(
      ParSet(
        List[Par](GInt(1L), GInt(2L), GInt(3L))
      )
    )
    result.exprs should be(Seq(Expr(resultSet)))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b', 3: 'c'}.size()" should "return 3" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
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
        reducer.evalExpr(EMethodBody(EMethod("size", map)))
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2, 3).size()" should "return 3" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()

        val set = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        reducer.evalExpr(EMethodBody(EMethod("size", set)))
    }
    result.exprs should be(Seq(Expr(GInt(3L))))
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2) + 3" should "return Set(1, 2, 3)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        reducer.evalExpr(EPlusBody(EPlus(set, GInt(3L))))
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
    result.exprs should be(Seq(Expr(resultSet)))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b', 3: 'c'} - 3" should "return {1: 'a', 2: 'b'}" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
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
        reducer.evalExpr(
          EMinusBody(EMinus(map, GInt(3L)))
        )
    }
    val resultMap =
      EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
    result.exprs should be(Seq(Expr(resultMap)))
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2, 3) - 3" should "return Set(1, 2)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        reducer.evalExpr(EMinusBody(EMinus(set, GInt(3L))))
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
    result.exprs should be(Seq(Expr(resultSet)))
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2) ++ Set(3, 4)" should "return Set(1, 2, 3, 4)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        val rhsSet       = ESetBody(ParSet(List[Par](GInt(3L), GInt(4L))))
        reducer.evalExpr(EPlusPlusBody(EPlusPlus(lhsSet, rhsSet)))
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L), GInt(4L))))
    result.exprs should be(Seq(Expr(resultSet)))
    error.get.unsafeRunSync should be(None)
  }

  "{1: 'a', 2: 'b'} ++ {3: 'c', 4: 'd'}" should "return union" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsMap =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        val rhsMap =
          EMapBody(ParMap(List[(Par, Par)]((GInt(3L), GString("c")), (GInt(4L), GString("d")))))
        reducer.evalExpr(EPlusPlusBody(EPlusPlus(lhsMap, rhsMap)))
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
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2, 3, 4) -- Set(1, 2)" should "return Set(3, 4)" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    val result = fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val lhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L), GInt(4L))))
        val rhsSet       = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L))))
        reducer.evalExpr(EMinusMinusBody(EMinusMinus(lhsSet, rhsSet)))
    }
    val resultSet = ESetBody(ParSet(List[Par](GInt(3L), GInt(4L))))
    result.exprs should be(Seq(Expr(resultSet)))
    error.get.unsafeRunSync should be(None)
  }

  "Set(1, 2, 3).get(1)" should "not work" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val set          = ESetBody(ParSet(List[Par](GInt(1L), GInt(2L), GInt(3L))))
        reducer.eval(EMethodBody(EMethod("get", set, List(GInt(1L)))))
    }
    error.get.unsafeRunSync should be(
      Some(MethodNotDefined("get", "Set"))
    )
  }

  "{1: 'a', 2: 'b'}.add(1)" should "not work" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()

    fixture {
      case (_, reducer) =>
        implicit val env = Env.makeEnv[Par]()
        val map =
          EMapBody(ParMap(List[(Par, Par)]((GInt(1L), GString("a")), (GInt(2L), GString("b")))))
        reducer.eval(EMethodBody(EMethod("add", map, List(GInt(1L)))))
    }
    error.get.unsafeRunSync should be(
      Some(MethodNotDefined("add", "Map"))
    )
  }

  it should "return an error when `toList` is called with arguments" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toListCall: EMethod =
      EMethod(
        "toList",
        EList(List()),
        List[Par](GInt(1L))
      )

    val result = fixture {
      case (space, reducer) =>
        implicit val env = Env[Par]()
        val nthTask      = reducer.eval(toListCall)
        nthTask >> space.toMap
    }
    result should be(mutable.HashMap.empty)
    error.get.unsafeRunSync should be(
      Some(MethodArgumentNumberMismatch("toList", 0, 1))
    )
  }

  it should "transform Set(1, 2, 3) into a [1, 2, 3]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toListCall: EMethod =
      EMethod(
        "toList",
        ESetBody(
          ParSet(
            List(
              GInt(1L),
              GInt(2L),
              GInt(3L)
            )
          )
        ),
        List[Par]()
      )

    val result: Par = fixture {
      case (_, reducer) =>
        implicit val env: Env[Par] = Env[Par]()
        reducer.evalExpr(toListCall)
    }
    val resultList =
      EListBody(
        EList(
          List[Par](
            GInt(1L),
            GInt(2L),
            GInt(3L)
          )
        )
      )
    result.exprs should be(Seq(Expr(resultList)))
    error.get.unsafeRunSync should be(None)
  }

  it should "transform {'a':1, 'b':2, 'c':3} into [('a',1), ('b',2), ('c',3)]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toListCall: EMethod =
      EMethod(
        "toList",
        EMapBody(
          ParMap(
            List[(Par, Par)](
              (GString("a"), GInt(1L)),
              (GString("b"), GInt(2L)),
              (GString("c"), GInt(3L))
            )
          )
        ),
        List[Par]()
      )
    val result: Par = fixture {
      case (_, reducer) =>
        implicit val env: Env[Par] = Env[Par]()
        reducer.evalExpr(toListCall)
    }
    val resultList =
      EListBody(
        EList(
          List[Par](
            ETupleBody(ETuple(Seq(GString("a"), GInt(1L)))),
            ETupleBody(ETuple(Seq(GString("b"), GInt(2L)))),
            ETupleBody(ETuple(Seq(GString("c"), GInt(3L))))
          )
        )
      )
    result.exprs should be(Seq(Expr(resultList)))
    error.get.unsafeRunSync should be(None)
  }

  it should "transform (1, 2, 3) into [1, 2, 3]" in {
    implicit val error = ErrorHandling.emptyError[Task].runSyncUnsafe()
    val toListCall: EMethod =
      EMethod(
        "toList",
        ETupleBody(
          ETuple(
            List[Par](
              GInt(1L),
              GInt(2L),
              GInt(3L)
            )
          )
        ),
        List[Par]()
      )
    val result: Par = fixture {
      case (_, reducer) =>
        implicit val env: Env[Par] = Env[Par]()
        reducer.evalExpr(toListCall)
    }
    val resultList =
      EListBody(
        EList(
          List[Par](
            GInt(1L),
            GInt(2L),
            GInt(3L)
          )
        )
      )
    result.exprs should be(Seq(Expr(resultList)))
    error.get.unsafeRunSync should be(None)
  }

  val successfulExamples = Table(
    ("clue", "input", "output"),
    (
      "[1, 2, 3].toSet() => Set(1, 2, 3)",
      EMethod(
        "toSet",
        EListBody(
          EList(
            List[Par](
              GInt(1L),
              GInt(2L),
              GInt(3L)
            )
          )
        ),
        List[Par]()
      ),
      ESetBody(
        ParSet(
          List(
            GInt(1L),
            GInt(2L),
            GInt(3L)
          )
        )
      )
    ),
    (
      "[1, 1].toSet() => Set(1)",
      EMethod(
        "toSet",
        EListBody(
          EList(
            List[Par](
              GInt(1L),
              GInt(1L)
            )
          )
        ),
        List[Par]()
      ),
      ESetBody(
        ParSet(
          List(
            GInt(1L)
          )
        )
      )
    ),
    (
      "[].toSet() => Set()",
      EMethod(
        "toSet",
        EListBody(
          EList(
            List[Par](
              )
          )
        ),
        List()
      ),
      ESetBody(
        ParSet(
          List(
            )
        )
      )
    ),
    (
      """[("a",1), ("b",2), ("c",3)].toMap() => {"a":1, "b":2, "c":3}""",
      EMethod(
        "toMap",
        EListBody(
          EList(
            List[Par](
              ETupleBody(ETuple(Seq(GString("a"), GInt(1L)))),
              ETupleBody(ETuple(Seq(GString("b"), GInt(2L)))),
              ETupleBody(ETuple(Seq(GString("c"), GInt(3L))))
            )
          )
        ),
        List[Par]()
      ),
      EMapBody(
        ParMap(
          List[(Par, Par)](
            (GString("a"), GInt(1L)),
            (GString("b"), GInt(2L)),
            (GString("c"), GInt(3L))
          )
        )
      )
    ),
    (
      """Set(("a",1), ("b",2), ("c",3)).toMap() => {"a":1, "b":2, "c":3}""",
      EMethod(
        "toMap",
        ESetBody(
          ParSet(
            List[Par](
              ETupleBody(ETuple(Seq(GString("a"), GInt(1L)))),
              ETupleBody(ETuple(Seq(GString("b"), GInt(2L)))),
              ETupleBody(ETuple(Seq(GString("c"), GInt(3L))))
            )
          )
        ),
        List[Par]()
      ),
      EMapBody(
        ParMap(
          List[(Par, Par)](
            (GString("a"), GInt(1L)),
            (GString("b"), GInt(2L)),
            (GString("c"), GInt(3L))
          )
        )
      )
    ),
    (
      """{"a":1, "b":2, "c":3}.toSet() => Set(("a",1), ("b",2), ("c",3))""",
      EMethod(
        "toSet",
        EMapBody(
          ParMap(
            List[(Par, Par)](
              (GString("a"), GInt(1L)),
              (GString("b"), GInt(2L)),
              (GString("c"), GInt(3L))
            )
          )
        ),
        List[Par]()
      ),
      ESetBody(
        ParSet(
          List[Par](
            ETupleBody(ETuple(Seq(GString("a"), GInt(1L)))),
            ETupleBody(ETuple(Seq(GString("b"), GInt(2L)))),
            ETupleBody(ETuple(Seq(GString("c"), GInt(3L))))
          )
        )
      )
    ),
    (
      """[("a",1), ("a",2)].toMap() => {"a":2)""",
      EMethod(
        "toMap",
        EListBody(
          EList(
            List[Par](
              ETupleBody(ETuple(Seq(GString("a"), GInt(1L)))),
              ETupleBody(ETuple(Seq(GString("a"), GInt(2L))))
            )
          )
        ),
        List[Par]()
      ),
      EMapBody(
        ParMap(
          List[(Par, Par)](
            (GString("a"), GInt(2L))
          )
        )
      )
    ),
    (
      """[].toMap() => {}""",
      EMethod(
        "toMap",
        EListBody(
          EList(
            List[Par](
              )
          )
        ),
        List()
      ),
      EMapBody(
        ParMap(
          List(
            )
        )
      )
    )
  )

  "Reducer" should "work correctly in succesful cases" in {
    forAll(successfulExamples) { (clue, input, output) =>
      val result = runReducer(input).map(_.exprs)

      result should be(Right(Seq(Expr(output)))) withClue (clue)
    }
  }

  val idempotenceExamples = Table(
    ("method", "input"),
    (
      "toSet",
      ESetBody(
        ParSet(
          List(
            GInt(1L),
            GInt(2L),
            GInt(3L)
          )
        )
      )
    ),
    (
      "toMap",
      EMapBody(
        ParMap(
          List[(Par, Par)](
            (GString("a"), GInt(1L)),
            (GString("b"), GInt(2L)),
            (GString("c"), GInt(3L))
          )
        )
      )
    )
  )
  "Idempotent methods" should "return the input" in {
    forAll(idempotenceExamples) { (method, input) =>
      val methodCall =
        EMethod(
          method,
          input,
          List()
        )

      val result = runReducer(methodCall).map(_.exprs)

      result should be(Right(Seq(Expr(input)))) withClue (s"$method should not change the object it is applied on")
    }
  }

  val errorExamples = Table(
    ("clue", "input", "output"),
    (
      """["a", ("b",2)].toMap() => MethodNotDefined""",
      EMethod(
        "toMap",
        EListBody(
          EList(
            List[Par](
              GString("a"),
              ETupleBody(ETuple(Seq(GString("b"), GInt(2L))))
            )
          )
        ),
        List[Par]()
      ),
      MethodNotDefined("toMap", "types except List[(K,V)]")
    ),
    (
      """[("a", 1)].toMap(1) => MethodArgumentNumberMismatch""",
      EMethod(
        "toMap",
        EListBody(
          EList(
            List[Par](
              ETupleBody(ETuple(Seq(GString("a"), GInt(1L))))
            )
          )
        ),
        List(GInt(1))
      ),
      MethodArgumentNumberMismatch("toMap", 0, 1)
    ),
    (
      "1.toMap() => MethodNotDefined",
      EMethod(
        "toMap",
        GInt(1L),
        List()
      ),
      MethodNotDefined("toMap", "Int")
    ),
    (
      "[].toSet(1) => MethodArgumentNumberMismatch",
      EMethod(
        "toSet",
        ESetBody(
          ParSet(
            List(
              )
          )
        ),
        List(GInt(1))
      ),
      MethodArgumentNumberMismatch("toSet", 0, 1)
    ),
    (
      "1.toSet() => MethodNotDefined",
      EMethod(
        "toSet",
        GInt(1L),
        List()
      ),
      MethodNotDefined("toSet", "Int")
    )
  )

  "Reducer" should "return report errors in failure cases" in {
    forAll(errorExamples) { (clue, input, error) =>
      runReducer(input) should be(Left(error)) withClue (clue)
    }
  }

  type RSpaceMap = Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]

  def runReducer(input: Par, timeout: Duration = 3.seconds): Either[Throwable, Par] = {
    implicit val error: _error[Task] = ErrorHandling.emptyError[Task].runSyncUnsafe()
    fixture {
      case (_, reducer) =>
        implicit val env: Env[Par] = Env[Par]()
        reducer.evalExpr(input).attempt
    }
  }
}
