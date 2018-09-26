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
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.IReplaySpace.IdIReplaySpace
import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.pure.PureRSpace
import coop.rchain.rspace.spaces.FineGrainedReplayRSpace
import coop.rchain.shared.StoreType
import coop.rchain.shared.StoreType._
import monix.eval.Task

import scala.collection.immutable

class Runtime private (
    val reducer: ChargingReducer[Task],
    val replayReducer: ChargingReducer[Task],
    val space: RhoISpace,
    val replaySpace: RhoReplayISpace,
    var errorLog: ErrorLog,
    val context: RhoContext
) {
  def readAndClearErrorVector(): Vector[Throwable] = errorLog.readAndClearErrorVector()
  def close(): Unit = {
    space.close()
    replaySpace.close()
    context.close()
  }
}

object Runtime {

  type RhoISpace          = CPARK[IdISpace]
  type RhoPureSpace[F[_]] = TCPARK[F, PureRSpace]
  type RhoReplayISpace    = CPARK[IdIReplaySpace]

  type RhoIStore  = CPAK[IStore]
  type RhoContext = CPAK[Context]

  type RhoDispatch[F[_]] = Dispatch[F, ListChannelWithRandom, TaggedContinuation]
  type RhoSysFunction    = Function1[Seq[ListChannelWithRandom], Task[Unit]]
  type RhoDispatchMap    = Map[Long, RhoSysFunction]

  private type CPAK[F[_, _, _, _]] =
    F[Channel, BindPattern, ListChannelWithRandom, TaggedContinuation]

  private type CPARK[F[_, _, _, _, _, _]] =
    F[
      Channel,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListChannelWithRandom,
      ListChannelWithRandom,
      TaggedContinuation
    ]

  private type TCPARK[M[_], F[_[_], _, _, _, _, _, _]] =
    F[
      M,
      Channel,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListChannelWithRandom,
      ListChannelWithRandom,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type Ref       = Long

  object BodyRefs {
    val STDOUT: Long                              = 0L
    val STDOUT_ACK: Long                          = 1L
    val STDERR: Long                              = 2L
    val STDERR_ACK: Long                          = 3L
    val ED25519_VERIFY: Long                      = 4L
    val SHA256_HASH: Long                         = 5L
    val KECCAK256_HASH: Long                      = 6L
    val BLAKE2B256_HASH: Long                     = 7L
    val SECP256K1_VERIFY: Long                    = 9L
    val REG_LOOKUP: Long                          = 10L
    val REG_LOOKUP_CALLBACK: Long                 = 11L
    val REG_INSERT: Long                          = 12L
    val REG_INSERT_CALLBACK: Long                 = 13L
    val REG_DELETE: Long                          = 14L
    val REG_DELETE_ROOT_CALLBACK: Long            = 15L
    val REG_DELETE_CALLBACK: Long                 = 16L
    val REG_PUBLIC_LOOKUP: Long                   = 17L
    val REG_PUBLIC_REGISTER_RANDOM: Long          = 18L
    val REG_PUBLIC_REGISTER_INSERT_CALLBACK: Long = 19L
  }

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  object FixedChannels {
    val STDOUT: Par            = byteName(0)
    val STDOUT_ACK: Par        = byteName(1)
    val STDERR: Par            = byteName(2)
    val STDERR_ACK: Par        = byteName(3)
    val ED25519_VERIFY: Par    = GString("ed25519Verify")
    val SHA256_HASH: Par       = GString("sha256Hash")
    val KECCAK256_HASH: Par    = GString("keccak256Hash")
    val BLAKE2B256_HASH: Par   = GString("blake2b256Hash")
    val SECP256K1_VERIFY: Par  = GString("secp256k1Verify")
    val REG_LOOKUP: Par        = byteName(9)
    val REG_INSERT_RANDOM: Par = byteName(10)
  }

  private def introduceSystemProcesses(
      space: RhoISpace,
      replaySpace: RhoISpace,
      processes: immutable.Seq[(Name, Arity, Remainder, Ref)]
  ): Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
    processes.flatMap {
      case (name, arity, remainder, ref) =>
        val channels = List(Channel(Quote(name)))
        val patterns = List(
          BindPattern(
            (0 until arity).map[Channel, Seq[Channel]](i => ChanVar(FreeVar(i))),
            remainder,
            freeCount = arity
          )
        )
        val continuation = TaggedContinuation(ScalaBodyRef(ref))
        Seq(
          space.install(channels, patterns, continuation),
          replaySpace.install(channels, patterns, continuation)
        )
    }

  /**
    * TODO this needs to go away when double locking is good enough
    */
  def setupRSpace(
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType
  ): (RhoContext, RhoISpace, RhoReplayISpace) = {
    def createCoarseRSpace(context: RhoContext): (RhoContext, RhoISpace, RhoReplayISpace) = {
      val space: RhoISpace             = RSpace.create(context, Branch.MASTER)
      val replaySpace: RhoReplayISpace = ReplayRSpace.create(context, Branch.REPLAY)
      (context, space, replaySpace)
    }
    storeType match {
      case InMem =>
        createCoarseRSpace(Context.createInMemory())
      case LMDB =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createCoarseRSpace(Context.create(dataDir, mapSize, true))
      case FineGrainedLMDB =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        val context: RhoContext          = Context.createFineGrained(dataDir, mapSize)
        val store                        = context.createStore(Branch.MASTER)
        val space: RhoISpace             = RSpace.createFineGrained(store, Branch.MASTER)
        val replaySpace: RhoReplayISpace = FineGrainedReplayRSpace.create(context, Branch.REPLAY)
        (context, space, replaySpace)
      case Mixed =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createCoarseRSpace(Context.createMixed(dataDir, mapSize))
    }
  }

  // TODO: remove default store type
  def create(dataDir: Path, mapSize: Long, storeType: StoreType = FineGrainedLMDB): Runtime = {
    val (context, space, replaySpace) = setupRSpace(dataDir, mapSize, storeType)

    val errorLog                                  = new ErrorLog()
    implicit val ft: FunctorTell[Task, Throwable] = errorLog

    def dispatchTableCreator(
        space: RhoISpace,
        dispatcher: RhoDispatch[Task],
        registry: Registry[Task]
    ): RhoDispatchMap = {
      import BodyRefs._
      Map(
        STDOUT                     -> SystemProcesses.stdout,
        STDOUT_ACK                 -> SystemProcesses.stdoutAck(space, dispatcher),
        STDERR                     -> SystemProcesses.stderr,
        STDERR_ACK                 -> SystemProcesses.stderrAck(space, dispatcher),
        ED25519_VERIFY             -> SystemProcesses.ed25519Verify(space, dispatcher),
        SHA256_HASH                -> SystemProcesses.sha256Hash(space, dispatcher),
        KECCAK256_HASH             -> SystemProcesses.keccak256Hash(space, dispatcher),
        BLAKE2B256_HASH            -> SystemProcesses.blake2b256Hash(space, dispatcher),
        SECP256K1_VERIFY           -> SystemProcesses.secp256k1Verify(space, dispatcher),
        REG_LOOKUP                 -> (registry.lookup(_)),
        REG_LOOKUP_CALLBACK        -> (registry.lookupCallback(_)),
        REG_INSERT                 -> (registry.insert(_)),
        REG_INSERT_CALLBACK        -> (registry.insertCallback(_)),
        REG_DELETE                 -> (registry.delete(_)),
        REG_DELETE_ROOT_CALLBACK   -> (registry.deleteRootCallback(_)),
        REG_DELETE_CALLBACK        -> (registry.deleteCallback(_)),
        REG_PUBLIC_LOOKUP          -> (registry.publicLookup(_)),
        REG_PUBLIC_REGISTER_RANDOM -> (registry.publicRegisterRandom(_))
      )
    }

    val urnMap: Map[String, Par] = Map(
      "rho:io:stdout"                -> FixedChannels.STDOUT,
      "rho:io:stdoutAck"             -> FixedChannels.STDOUT_ACK,
      "rho:io:stderr"                -> FixedChannels.STDERR,
      "rho:io:stderrAck"             -> FixedChannels.STDERR_ACK,
      "rho:registry:lookup"          -> FixedChannels.REG_LOOKUP,
      "rho:registry:insertArbitrary" -> FixedChannels.REG_INSERT_RANDOM
    )

    lazy val dispatchTable: RhoDispatchMap =
      dispatchTableCreator(space, dispatcher, registry)

    lazy val replayDispatchTable: RhoDispatchMap =
      dispatchTableCreator(replaySpace, replayDispatcher, replayRegistry)

    lazy val (dispatcher, reducer, registry) =
      RholangAndScalaDispatcher.create(space, dispatchTable, urnMap)

    lazy val (replayDispatcher, replayReducer, replayRegistry) =
      RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable, urnMap)

    val procDefs: immutable.Seq[(Name, Arity, Remainder, Ref)] = {
      import BodyRefs._
      List(
        (FixedChannels.STDOUT, 1, None, STDOUT),
        (FixedChannels.STDOUT_ACK, 2, None, STDOUT_ACK),
        (FixedChannels.STDERR, 1, None, STDERR),
        (FixedChannels.STDERR_ACK, 2, None, STDERR_ACK),
        (FixedChannels.ED25519_VERIFY, 4, None, ED25519_VERIFY),
        (FixedChannels.SHA256_HASH, 2, None, SHA256_HASH),
        (FixedChannels.KECCAK256_HASH, 2, None, KECCAK256_HASH),
        (FixedChannels.BLAKE2B256_HASH, 2, None, BLAKE2B256_HASH),
        (FixedChannels.SECP256K1_VERIFY, 4, None, SECP256K1_VERIFY),
        (FixedChannels.REG_LOOKUP, 2, None, REG_PUBLIC_LOOKUP),
        (FixedChannels.REG_INSERT_RANDOM, 2, None, REG_PUBLIC_REGISTER_RANDOM)
      )
    }

    val res: Seq[Option[(TaggedContinuation, Seq[ListChannelWithRandom])]] =
      introduceSystemProcesses(space, replaySpace, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(reducer, replayReducer, space, replaySpace, errorLog, context)
  }
}
