package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats.mtl.FunctorTell
import com.google.protobuf.ByteString
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

  type RhoDispatch    = Dispatch[Task, ListChannelWithRandom, TaggedContinuation]
  type RhoSysFunction = Function1[Seq[ListChannelWithRandom], Task[Unit]]
  type RhoDispatchMap = Map[Long, RhoSysFunction]

  private type CPAK[F[_, _, _, _]] =
    F[Channel, BindPattern, ListChannelWithRandom, TaggedContinuation]

  private type CPARK[F[_, _, _, _, _]] =
    F[Channel, BindPattern, ListChannelWithRandom, ListChannelWithRandom, TaggedContinuation]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type Ref       = Long

  object BodyRefs {
    val STDOUT: Long                   = 0L
    val STDOUT_ACK: Long               = 1L
    val STDERR: Long                   = 2L
    val STDERR_ACK: Long               = 3L
    val ED25519_VERIFY: Long           = 4L
    val SHA256_HASH: Long              = 5L
    val KECCAK256_HASH: Long           = 6L
    val BLAKE2B256_HASH: Long          = 7L
    val SECP256K1_VERIFY: Long         = 9L
    val REG_LOOKUP: Long               = 10L
    val REG_LOOKUP_CALLBACK: Long      = 11L
    val REG_INSERT: Long               = 12L
    val REG_INSERT_CALLBACK: Long      = 13L
    val REG_DELETE: Long               = 14L
    val REG_DELETE_ROOT_CALLBACK: Long = 15L
    val REG_DELETE_CALLBACK: Long      = 16L
  }

  private def introduceSystemProcesses(space: RhoISpace,
                                       replaySpace: RhoISpace,
                                       processes: immutable.Seq[(Name, Arity, Remainder, Ref)])
    : Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
    processes.flatMap {
      case (name, arity, remainder, ref) =>
        val channels = List(Channel(Quote(name)))
        val patterns = List(
          BindPattern((0 until arity).map[Channel, Seq[Channel]](i => ChanVar(FreeVar(i))),
                      remainder,
                      freeCount = arity))
        val continuation = TaggedContinuation(ScalaBodyRef(ref))
        Seq(
          space.install(channels, patterns, continuation),
          replaySpace.install(channels, patterns, continuation)
        )
    }

  def create(dataDir: Path, mapSize: Long, inMemoryStore: Boolean = false): Runtime = {
    val context: RhoContext = if (inMemoryStore) {
      Context.createInMemory()
    } else {
      if (Files.notExists(dataDir)) {
        Files.createDirectories(dataDir)
      }
      Context.create(dataDir, mapSize, true)
    }

    val space: RhoRSpace             = RSpace.create(context, Branch.MASTER)
    val replaySpace: RhoReplayRSpace = ReplayRSpace.create(context, Branch.REPLAY)

    val errorLog                                  = new ErrorLog()
    implicit val ft: FunctorTell[Task, Throwable] = errorLog

    def dispatchTableCreator(space: RhoISpace, dispatcher: RhoDispatch): RhoDispatchMap = {
      import BodyRefs._
      val registry = new Registry(space, dispatcher)
      Map(
        STDOUT                   -> SystemProcesses.stdout,
        STDOUT_ACK               -> SystemProcesses.stdoutAck(space, dispatcher),
        STDERR                   -> SystemProcesses.stderr,
        STDERR_ACK               -> SystemProcesses.stderrAck(space, dispatcher),
        ED25519_VERIFY           -> SystemProcesses.ed25519Verify(space, dispatcher),
        SHA256_HASH              -> SystemProcesses.sha256Hash(space, dispatcher),
        KECCAK256_HASH           -> SystemProcesses.keccak256Hash(space, dispatcher),
        BLAKE2B256_HASH          -> SystemProcesses.blake2b256Hash(space, dispatcher),
        SECP256K1_VERIFY         -> SystemProcesses.secp256k1Verify(space, dispatcher),
        REG_LOOKUP               -> (registry.lookup(_)),
        REG_LOOKUP_CALLBACK      -> (registry.lookupCallback(_)),
        REG_INSERT               -> (registry.insert(_)),
        REG_INSERT_CALLBACK      -> (registry.insertCallback(_)),
        REG_DELETE               -> (registry.delete(_)),
        REG_DELETE_ROOT_CALLBACK -> (registry.deleteRootCallback(_)),
        REG_DELETE_CALLBACK      -> (registry.deleteCallback(_))
      )
    }

    def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

    val urnMap: Map[String, Par] = Map("rho:io:stdout" -> byteName(0),
                                       "rho:io:stdoutAck" -> byteName(1),
                                       "rho:io:stderr"    -> byteName(2),
                                       "rho:io:stderrAck" -> byteName(3))

    lazy val dispatchTable: RhoDispatchMap =
      dispatchTableCreator(space, dispatcher)

    lazy val replayDispatchTable: RhoDispatchMap =
      dispatchTableCreator(replaySpace, replayDispatcher)

    lazy val dispatcher: RhoDispatch =
      RholangAndScalaDispatcher.create(space, dispatchTable, urnMap)

    lazy val replayDispatcher: RhoDispatch =
      RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable, urnMap)

    val procDefs: immutable.Seq[(Name, Arity, Remainder, Ref)] = {
      import BodyRefs._
      List(
        (byteName(0), 1, None, STDOUT),
        (byteName(1), 2, None, STDOUT_ACK),
        (byteName(2), 1, None, STDERR),
        (byteName(3), 2, None, STDERR_ACK),
        (GString("ed25519Verify"), 4, None, ED25519_VERIFY),
        (GString("sha256Hash"), 2, None, SHA256_HASH),
        (GString("keccak256Hash"), 2, None, KECCAK256_HASH),
        (GString("blake2b256Hash"), 2, None, BLAKE2B256_HASH),
        (GString("secp256k1Verify"), 4, None, SECP256K1_VERIFY)
      )
    }

    val res: Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
      introduceSystemProcesses(space, replaySpace, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(dispatcher.reducer, replayDispatcher.reducer, space, replaySpace, errorLog, context)
  }
}
