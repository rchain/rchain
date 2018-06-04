package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import coop.rchain.catscontrib.Capture
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models.{BindPattern, Channel, TaggedContinuation, Var}
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{install, IStore, LMDBStore}
import monix.eval.Task

import scala.collection.immutable

class Runtime private (val reducer: Reduce[Task],
                       val store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation])

object Runtime {

  type Name      = String
  type Arity     = Int
  type Remainder = Option[Var]
  type Ref       = Long

  private def introduceSystemProcesses(
      store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
      processes: immutable.Seq[(Name, Arity, Remainder, Ref)])
    : Seq[Option[(TaggedContinuation, Seq[Seq[Channel]])]] =
    processes.map {
      case (name, arity, remainder, ref) =>
        install(
          store,
          List(Channel(Quote(GString(name)))),
          List(
            BindPattern((0 until arity).map[Channel, Seq[Channel]](i => ChanVar(FreeVar(i))),
                        remainder,
                        freeCount = arity)),
          TaggedContinuation(ScalaBodyRef(ref))
        )
    }

  def create(dataDir: Path, mapSize: Long)(implicit captureTask: Capture[Task]): Runtime = {

    if (Files.notExists(dataDir)) Files.createDirectories(dataDir)

    val store =
      LMDBStore
        .create[Channel, BindPattern, Seq[Channel], TaggedContinuation](dataDir, mapSize)

    lazy val dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation] =
      RholangAndScalaDispatcher.create(store, dispatchTable)

    lazy val dispatchTable: Map[Ref, Seq[Seq[Channel]] => Task[Unit]] = Map(
      0L -> SystemProcesses.stdout,
      1L -> SystemProcesses.stdoutAck(store, dispatcher),
      2L -> SystemProcesses.stderr,
      3L -> SystemProcesses.stderrAck(store, dispatcher),
      4L -> SystemProcesses.ed25519Verify(store, dispatcher),
      5L -> SystemProcesses.sha256Hash(store, dispatcher),
      6L -> SystemProcesses.keccak256Hash(store, dispatcher),
      7L -> SystemProcesses.blake2b256Hash(store, dispatcher)
      //TODO: once we have secp256k1 packaged as jar
//      9L -> SystemProcesses.secp256k1Verify(store, dispatcher)
    )

    val procDefs: immutable.Seq[(Name, Arity, Remainder, Ref)] = List(
      ("stdout", 1, None, 0L),
      ("stdoutAck", 2, None, 1L),
      ("stderr", 1, None, 2L),
      ("stderrAck", 2, None, 3L),
      ("ed25519Verify", 4, None, 4L),
      ("sha256Hash", 2, None, 5L),
      ("keccak256Hash", 2, None, 6L),
      ("blake2b256Hash", 2, None, 7L)
      //TODO: once we have secp256k1 packaged as jar
//      ("secp256k1Verify", 4, None, 9L)
    )

    val res: Seq[Option[(TaggedContinuation, Seq[Seq[Channel]])]] =
      introduceSystemProcesses(store, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(dispatcher.reducer, store)
  }
}
