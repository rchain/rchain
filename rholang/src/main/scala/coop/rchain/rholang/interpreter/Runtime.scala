package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats.mtl.{FunctorTell, MonadState}
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{BindPattern, Channel, TaggedContinuation, Var}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.shared.AtomicRefMonadState
import monix.eval.Task

import scala.collection.immutable

class Runtime private (
    val reducer: Reduce[Task],
    val replayReducer: Reduce[Task],
    val space: ISpace[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation],
    val replaySpace: ReplayRSpace[Channel,
                                  BindPattern,
                                  Seq[Channel],
                                  Seq[Channel],
                                  TaggedContinuation],
    val costAccounting: CostAccountingAlg[Task],
    var errorLog: ErrorLog) {
  def readAndClearErrorVector(): Vector[Throwable] = errorLog.readAndClearErrorVector()
  def getCost(): Task[CostAccount]                 = costAccounting.getTotal
  def close(): Unit                                = space.close()
}

object Runtime {

  type Name      = String
  type Arity     = Int
  type Remainder = Option[Var]
  type Ref       = Long

  private def introduceSystemProcesses(
      space: ISpace[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation],
      processes: immutable.Seq[(Name, Arity, Remainder, Ref)])
    : Seq[Option[(TaggedContinuation, Seq[Seq[Channel]])]] =
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
    }

  def create(dataDir: Path, mapSize: Long): Runtime = {

    if (Files.notExists(dataDir)) Files.createDirectories(dataDir)

    val context = Context.create[Channel, BindPattern, Seq[Channel], TaggedContinuation](
      dataDir,
      mapSize
    )

    val space = RSpace.create[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation](
      context,
      Branch.MASTER)

    val replaySpace =
      ReplayRSpace.create[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation](
        context,
        Branch.REPLAY)

    val errorLog                                  = new ErrorLog()
    implicit val ft: FunctorTell[Task, Throwable] = errorLog
    val costState                                 = AtomicRefMonadState.of[Task, CostAccount](CostAccount.zero)
    val costAccounting: CostAccountingAlg[Task] =
      CostAccountingAlg.monadState(costState)

    lazy val dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation] =
      RholangAndScalaDispatcher
        .create(space, dispatchTable, costAccounting)

    lazy val replayDispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation] =
      RholangAndScalaDispatcher
        .create(replaySpace, dispatchTable, costAccounting)

    lazy val dispatchTable: Map[Ref, Seq[Seq[Channel]] => Task[Unit]] = Map(
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

    val res: Seq[Option[(TaggedContinuation, Seq[Seq[Channel]])]] =
      introduceSystemProcesses(space, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(dispatcher.reducer,
                replayDispatcher.reducer,
                space,
                replaySpace,
                costAccounting,
                errorLog)
  }
}
