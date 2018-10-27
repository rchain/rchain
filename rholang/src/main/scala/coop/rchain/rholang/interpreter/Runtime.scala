package coop.rchain.rholang.interpreter

import java.nio.file.{Files, Path}

import cats.Id
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl.FunctorTell
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.SetupError
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.IReplaySpace
import coop.rchain.rspace.ISpace
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
    val errorLog: ErrorLog,
    val context: RhoContext,
    val shortLeashParams: Runtime.ShortLeashParams[Task],
    val blockTime: Runtime.BlockTime[Task]
) {
  def readAndClearErrorVector(): Vector[Throwable] = errorLog.readAndClearErrorVector()
  def close(): Unit = {
    space.close()
    replaySpace.close()
    context.close()
  }
  def injectEmptyRegistryRoot[F[_]](implicit F: Sync[F]): F[Unit] = {
    // This random value stays dead in the tuplespace, so we can have some fun.
    // This is from Jeremy Bentham's "Defence of Usury"
    val rand = Blake2b512Random(
      ("there can be no such thing as usury: " +
        "for what rate of interest is there that can naturally be more proper than another?")
        .getBytes()
    )
    implicit val MATCH_UNLIMITED_PHLOS = matchListPar(Cost(Integer.MAX_VALUE))
    for {
      spaceResult <- F.delay(
                      space.produce(
                        Registry.registryRoot,
                        ListParWithRandom(Seq(Registry.emptyMap), rand),
                        false
                      )
                    )
      replayResult <- F.delay(
                       replaySpace.produce(
                         Registry.registryRoot,
                         ListParWithRandom(Seq(Registry.emptyMap), rand),
                         false
                       )
                     )
      _ <- spaceResult match {
            case Right(None) =>
              replayResult match {
                case Right(None) => F.unit
                case Right(Some(_)) =>
                  F.raiseError(
                    new SetupError("Registry insertion in replay fired continuation.")
                  )
                case Left(err) => F.raiseError(err)
              }
            case Right(Some(_)) =>
              F.raiseError(new SetupError("Registry insertion fired continuation."))
            case Left(err) => F.raiseError(err)
          }
    } yield ()
  }

}

object Runtime {

  type RhoISpace          = TCPARK[Id, ISpace]
  type RhoPureSpace[F[_]] = TCPARK[F, PureRSpace]
  type RhoReplayISpace    = TCPARK[Id, IReplaySpace]

  type RhoIStore  = CPAK[IStore]
  type RhoContext = CPAK[Context]

  type RhoDispatch[F[_]] = Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  type RhoSysFunction    = Function1[Seq[ListParWithRandomAndPhlos], Task[Unit]]
  type RhoDispatchMap    = Map[Long, RhoSysFunction]

  type CPAK[F[_, _, _, _]] =
    F[Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type CPARK[F[_, _, _, _, _, _]] =
    F[
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ]

  type TCPARK[M[_], F[_[_], _, _, _, _, _, _]] =
    F[
      M,
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type BodyRef   = Long

  class ShortLeashParams[F[_]](
      val codeHash: Ref[F, Par],
      var phloRate: Ref[F, Par],
      var userId: Ref[F, Par],
      var timestamp: Ref[F, Par]
  ) {
    def setParams(codeHash: Par, phloRate: Par, userId: Par, timestamp: Par)(implicit F: Sync[F]) =
      for {
        _ <- this.codeHash.set(codeHash)
        _ <- this.phloRate.set(phloRate)
        _ <- this.userId.set(userId)
        _ <- this.timestamp.set(timestamp)
      } yield ()
  }

  object ShortLeashParams {
    def apply[F[_]]()(implicit F: Sync[F]): F[ShortLeashParams[F]] =
      for {
        codeHash  <- Ref[F].of(Par())
        phloRate  <- Ref[F].of(Par())
        userId    <- Ref[F].of(Par())
        timestamp <- Ref[F].of(Par())
      } yield new ShortLeashParams[F](codeHash, phloRate, userId, timestamp)

    def unsafe[F[_]]()(implicit F: Sync[F]): ShortLeashParams[F] =
      new ShortLeashParams(
        Ref.unsafe[F, Par](Par()),
        Ref.unsafe[F, Par](Par()),
        Ref.unsafe[F, Par](Par()),
        Ref.unsafe[F, Par](Par())
      )
  }

  class BlockTime[F[_]](val timestamp: Ref[F, Par]) {
    def setParams(timestamp: Par)(implicit F: Sync[F]): F[Unit] =
      for {
        _ <- this.timestamp.set(timestamp)
      } yield ()
  }

  object BlockTime {
    def apply[F[_]]()(implicit F: Sync[F]): F[BlockTime[F]] =
      for {
        timestamp <- Ref[F].of(Par())
      } yield new BlockTime[F](timestamp)

    def unsafe[F[_]]()(implicit F: Sync[F]): BlockTime[F] =
      new BlockTime(Ref.unsafe[F, Par](Par()))
  }

  object BodyRefs {
    val STDOUT: Long                       = 0L
    val STDOUT_ACK: Long                   = 1L
    val STDERR: Long                       = 2L
    val STDERR_ACK: Long                   = 3L
    val ED25519_VERIFY: Long               = 4L
    val SHA256_HASH: Long                  = 5L
    val KECCAK256_HASH: Long               = 6L
    val BLAKE2B256_HASH: Long              = 7L
    val SECP256K1_VERIFY: Long             = 9L
    val REG_LOOKUP: Long                   = 10L
    val REG_LOOKUP_CALLBACK: Long          = 11L
    val REG_INSERT: Long                   = 12L
    val REG_INSERT_CALLBACK: Long          = 13L
    val REG_DELETE: Long                   = 14L
    val REG_DELETE_ROOT_CALLBACK: Long     = 15L
    val REG_DELETE_CALLBACK: Long          = 16L
    val REG_PUBLIC_LOOKUP: Long            = 17L
    val REG_PUBLIC_REGISTER_RANDOM: Long   = 18L
    val REG_REGISTER_INSERT_CALLBACK: Long = 19L
    val REG_PUBLIC_REGISTER_SIGNED: Long   = 20L
    val REG_NONCE_INSERT_CALLBACK: Long    = 21L
    val GET_DEPLOY_PARAMS: Long            = 22L
    val GET_TIMESTAMP: Long                = 23L
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
    val REG_INSERT_SIGNED: Par = byteName(11)
    val GET_DEPLOY_PARAMS: Par = byteName(12)
    val GET_TIMESTAMP: Par     = byteName(13)
  }

  // because only we do installs
  private val MATCH_UNLIMITED_PHLOS = matchListPar(Cost(Integer.MAX_VALUE))

  private def introduceSystemProcesses(
      space: RhoISpace,
      replaySpace: RhoISpace,
      processes: immutable.Seq[(Name, Arity, Remainder, BodyRef)]
  ): Seq[Option[(TaggedContinuation, Seq[ListParWithRandomAndPhlos])]] =
    processes.flatMap {
      case (name, arity, remainder, ref) =>
        val channels = List(name)
        val patterns = List(
          BindPattern(
            (0 until arity).map[Par, Seq[Par]](i => EVar(FreeVar(i))),
            remainder,
            freeCount = arity
          )
        )
        val continuation = TaggedContinuation(ScalaBodyRef(ref))
        Seq(
          space.install(channels, patterns, continuation)(MATCH_UNLIMITED_PHLOS),
          replaySpace.install(channels, patterns, continuation)(MATCH_UNLIMITED_PHLOS)
        )
    }

  def setupRSpace(
      dataDir: Path,
      mapSize: Long,
      storeType: StoreType
  ): (RhoContext, RhoISpace, RhoReplayISpace) = {
    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    def createSpace(context: RhoContext): (RhoContext, RhoISpace, RhoReplayISpace) = {
      val space: RhoISpace = RSpace.create[
        Id,
        Par,
        BindPattern,
        OutOfPhlogistonsError.type,
        ListParWithRandom,
        ListParWithRandomAndPhlos,
        TaggedContinuation
      ](context, Branch.MASTER)
      val replaySpace: RhoReplayISpace = ReplayRSpace.create[
        Id,
        Par,
        BindPattern,
        OutOfPhlogistonsError.type,
        ListParWithRandom,
        ListParWithRandomAndPhlos,
        TaggedContinuation
      ](context, Branch.REPLAY)
      (context, space, replaySpace)
    }
    storeType match {
      case InMem =>
        createSpace(Context.createInMemory())
      case LMDB =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createSpace(Context.create(dataDir, mapSize, true))
      case Mixed =>
        if (Files.notExists(dataDir)) {
          Files.createDirectories(dataDir)
        }
        createSpace(Context.createMixed(dataDir, mapSize))
    }
  }

  // TODO: remove default store type
  def create(dataDir: Path, mapSize: Long, storeType: StoreType = LMDB): Runtime = {
    val (context, space, replaySpace) = setupRSpace(dataDir, mapSize, storeType)

    val errorLog                                  = new ErrorLog()
    implicit val ft: FunctorTell[Task, Throwable] = errorLog

    def dispatchTableCreator(
        space: RhoISpace,
        dispatcher: RhoDispatch[Task],
        registry: Registry[Task],
        shortLeashParams: ShortLeashParams[Task],
        blockTime: BlockTime[Task]
    ): RhoDispatchMap = {
      import BodyRefs._
      Map(
        STDOUT                       -> SystemProcesses.stdout,
        STDOUT_ACK                   -> SystemProcesses.stdoutAck(space, dispatcher),
        STDERR                       -> SystemProcesses.stderr,
        STDERR_ACK                   -> SystemProcesses.stderrAck(space, dispatcher),
        ED25519_VERIFY               -> SystemProcesses.ed25519Verify(space, dispatcher),
        SHA256_HASH                  -> SystemProcesses.sha256Hash(space, dispatcher),
        KECCAK256_HASH               -> SystemProcesses.keccak256Hash(space, dispatcher),
        BLAKE2B256_HASH              -> SystemProcesses.blake2b256Hash(space, dispatcher),
        SECP256K1_VERIFY             -> SystemProcesses.secp256k1Verify(space, dispatcher),
        REG_LOOKUP                   -> (registry.lookup(_)),
        REG_LOOKUP_CALLBACK          -> (registry.lookupCallback(_)),
        REG_INSERT                   -> (registry.insert(_)),
        REG_INSERT_CALLBACK          -> (registry.insertCallback(_)),
        REG_REGISTER_INSERT_CALLBACK -> (registry.registerInsertCallback(_)),
        REG_DELETE                   -> (registry.delete(_)),
        REG_DELETE_ROOT_CALLBACK     -> (registry.deleteRootCallback(_)),
        REG_DELETE_CALLBACK          -> (registry.deleteCallback(_)),
        REG_PUBLIC_LOOKUP            -> (registry.publicLookup(_)),
        REG_PUBLIC_REGISTER_RANDOM   -> (registry.publicRegisterRandom(_)),
        REG_PUBLIC_REGISTER_SIGNED   -> (registry.publicRegisterSigned(_)),
        REG_NONCE_INSERT_CALLBACK    -> (registry.nonceInsertCallback(_)),
        GET_DEPLOY_PARAMS            -> SystemProcesses.getDeployParams(space, dispatcher, shortLeashParams),
        GET_TIMESTAMP                -> SystemProcesses.blockTime(space, dispatcher, blockTime)
      )
    }

    val urnMap: Map[String, Par] = Map(
      "rho:io:stdout"                -> Bundle(FixedChannels.STDOUT, writeFlag = true),
      "rho:io:stdoutAck"             -> Bundle(FixedChannels.STDOUT_ACK, writeFlag = true),
      "rho:io:stderr"                -> Bundle(FixedChannels.STDERR, writeFlag = true),
      "rho:io:stderrAck"             -> Bundle(FixedChannels.STDERR_ACK, writeFlag = true),
      "rho:registry:lookup"          -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true),
      "rho:registry:insertArbitrary" -> Bundle(FixedChannels.REG_INSERT_RANDOM, writeFlag = true),
      "rho:registry:insertSigned:ed25519" -> Bundle(
        FixedChannels.REG_INSERT_SIGNED,
        writeFlag = true
      ),
      "rho:deploy:params"   -> Bundle(FixedChannels.GET_DEPLOY_PARAMS, writeFlag = true),
      "rho:block:timestamp" -> Bundle(FixedChannels.GET_TIMESTAMP, writeFlag = true)
    )

    val shortLeashParams = ShortLeashParams.unsafe[Task]()
    val blockTime        = BlockTime.unsafe[Task]()

    lazy val dispatchTable: RhoDispatchMap =
      dispatchTableCreator(space, dispatcher, registry, shortLeashParams, blockTime)

    lazy val replayDispatchTable: RhoDispatchMap =
      dispatchTableCreator(
        replaySpace,
        replayDispatcher,
        replayRegistry,
        shortLeashParams,
        blockTime
      )

    lazy val (dispatcher, reducer, registry) =
      RholangAndScalaDispatcher.create(space, dispatchTable, urnMap)

    lazy val (replayDispatcher, replayReducer, replayRegistry) =
      RholangAndScalaDispatcher.create(replaySpace, replayDispatchTable, urnMap)

    val procDefs: immutable.Seq[(Name, Arity, Remainder, BodyRef)] = {
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
        (FixedChannels.REG_INSERT_RANDOM, 2, None, REG_PUBLIC_REGISTER_RANDOM),
        (FixedChannels.REG_INSERT_SIGNED, 4, None, REG_PUBLIC_REGISTER_SIGNED),
        (FixedChannels.GET_DEPLOY_PARAMS, 1, None, GET_DEPLOY_PARAMS),
        (FixedChannels.GET_TIMESTAMP, 1, None, GET_TIMESTAMP)
      )
    }

    val res: Seq[Option[(TaggedContinuation, Seq[ListParWithRandomAndPhlos])]] =
      introduceSystemProcesses(space, replaySpace, procDefs)

    assert(res.forall(_.isEmpty))

    new Runtime(
      reducer,
      replayReducer,
      space,
      replaySpace,
      errorLog,
      context,
      shortLeashParams,
      blockTime
    )
  }
}
