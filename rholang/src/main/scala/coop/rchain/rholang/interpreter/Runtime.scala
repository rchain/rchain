package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats.mtl.FunctorTell
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import monix.eval.Task

import scala.collection.immutable

class Runtime private (val reducer: Reduce[Task],
                       val replayReducer: Reduce[Task],
                       val space: RhoISpace,
                       val replaySpace: RhoReplayRSpace,
                       var errorLog: ErrorLog,
                       val context: RhoContext) {
  def readAndClearErrorVector(): Vector[Throwable] = errorLog.readAndClearErrorVector()
  def close(): Unit = {
    space.close()
    replaySpace.close()
    context.close()
  }
}

object Runtime {

  type RhoISpace       = CPARK[ISpace]
  type RhoRSpace       = CPARK[RSpace]
  type RhoReplayRSpace = CPARK[ReplayRSpace]

  type RhoIStore  = CPAK[IStore]
  type RhoContext = CPAK[Context]

  private type CPAK[F[_, _, _, _]] =
    F[Channel, BindPattern, ListChannelWithRandom, TaggedContinuation]

  private type CPARK[F[_, _, _, _, _]] =
    F[Channel, BindPattern, ListChannelWithRandom, ListChannelWithRandom, TaggedContinuation]

  type Name      = String
  type Arity     = Int
  type Remainder = Option[Var]
  type Ref       = Long

  private def introduceSystemProcesses(space: RhoISpace,
                                       replaySpace: RhoISpace,
                                       processes: immutable.Seq[(Name, Arity, Remainder, Ref)])
    : Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
    processes.map {
      case (name, arity, remainder, ref) =>
        space.install(
          List(Channel(Quote(GString(name)))),
          List(
            BindPattern((0 until arity).map[Channel, Seq[Channel]](i => ChanVar(FreeVar(i))),
                        remainder,
                        freeCount = arity)),
          TaggedContinuation(ScalaBodyRef(ref))
        )
        replaySpace.install(
          List(Channel(Quote(GString(name)))),
          List(
            BindPattern((0 until arity).map[Channel, Seq[Channel]](i => ChanVar(FreeVar(i))),
                        remainder,
                        freeCount = arity)),
          TaggedContinuation(ScalaBodyRef(ref))
        )
    }

  def create(dataDir: Path, mapSize: Long): Runtime = {

    if (Files.notExists(dataDir)) Files.createDirectories(dataDir)

    val context: RhoContext = Context.create(
      dataDir,
      mapSize
    )

    val space: RhoRSpace = RSpace.create(context, Branch.MASTER)

    val replaySpace: RhoReplayRSpace = ReplayRSpace.create(context, Branch.REPLAY)

    val errorLog                                  = new ErrorLog()
    implicit val ft: FunctorTell[Task, Throwable] = errorLog

    def dispatchTableCreator(
        space: RhoISpace,
        dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation]) = Map(
      0L -> SystemProcesses.stdout,
      1L -> SystemProcesses.stdoutAck(space, dispatcher),
      2L -> SystemProcesses.stderr,
      3L -> SystemProcesses.stderrAck(space, dispatcher),
      4L -> SystemProcesses.ed25519Verify(space, dispatcher),
      5L -> SystemProcesses.sha256Hash(space, dispatcher),
      6L -> SystemProcesses.keccak256Hash(space, dispatcher),
      7L -> SystemProcesses.blake2b256Hash(space, dispatcher),
      9L -> SystemProcesses.secp256k1Verify(space, dispatcher)
    )

    lazy val dispatchTable: Map[Ref, Seq[ListChannelWithRandom] => Task[Unit]] =
      dispatchTableCreator(space, dispatcher)

    lazy val replayDispatchTable: Map[Ref, Seq[ListChannelWithRandom] => Task[Unit]] =
      dispatchTableCreator(replaySpace, replayDispatcher)

    lazy val dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation] =
      RholangAndScalaDispatcher.create(space, dispatchTable)

    lazy val replayDispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation] =
      RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable)

    val procDefs: immutable.Seq[(Name, Arity, Remainder, Ref)] = List(
      ("stdout", 1, None, 0L),
      ("stdoutAck", 2, None, 1L),
      ("stderr", 1, None, 2L),
      ("stderrAck", 2, None, 3L),
      ("ed25519Verify", 4, None, 4L),
      ("sha256Hash", 2, None, 5L),
      ("keccak256Hash", 2, None, 6L),
      ("blake2b256Hash", 2, None, 7L),
      ("secp256k1Verify", 4, None, 9L)
    )

    val res: Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
      introduceSystemProcesses(space, replaySpace, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(dispatcher.reducer, replayDispatcher.reducer, space, replaySpace, errorLog, context)
  }
}
