package coop.rchain.rholang.interpreter

import cats._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.Runtime._
import coop.rchain.rholang.interpreter.accounting.{noOpCostLog, _}
import coop.rchain.rholang.interpreter.registry.RegistryBootstrap
import coop.rchain.rholang.interpreter.storage.ChargingRSpace
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.{Match, RSpace, _}
import coop.rchain.shared.Log

import scala.concurrent.ExecutionContext

class Runtime[F[_]: Sync] private (
    val reducer: Reduce[F],
    val replayReducer: Reduce[F],
    val space: RhoISpace[F],
    val replaySpace: RhoReplayISpace[F],
    val cost: _cost[F],
    val blockData: Ref[F, BlockData],
    val invalidBlocks: InvalidBlocks[F]
) extends HasCost[F]

trait HasCost[F[_]] {
  def cost: _cost[F]
}

object Runtime {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(RholangMetricsSource, "runtime")

  type ISpaceAndReplay[F[_]] = (RhoISpace[F], RhoReplayISpace[F])
  type RhoHistoryRepository[F[_]] =
    HistoryRepository[F, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type RhoTuplespace[F[_]]   = TCPAK[F, Tuplespace]
  type RhoISpace[F[_]]       = TCPAK[F, ISpace]
  type RhoReplayISpace[F[_]] = TCPAK[F, IReplaySpace]

  type RhoDispatch[F[_]]    = Dispatch[F, ListParWithRandom, TaggedContinuation]
  type RhoSysFunction[F[_]] = Seq[ListParWithRandom] => F[Unit]
  type RhoDispatchMap[F[_]] = Map[Long, RhoSysFunction[F]]

  type CPAK[M[_], F[_[_], _, _, _, _]] =
    F[M, Par, BindPattern, ListParWithRandom, TaggedContinuation]

  type TCPAK[M[_], F[_[_], _, _, _, _]] =
    F[
      M,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ]

  type Name      = Par
  type Arity     = Int
  type Remainder = Option[Var]
  type BodyRef   = Long

  // seqNum is not exposed to rholang api contract currently
  final case class BlockData private (
      timeStamp: Long,
      blockNumber: Long,
      sender: PublicKey,
      seqNum: Int
  )
  object BlockData {
    def empty: BlockData = BlockData(0, 0, PublicKey(Base16.unsafeDecode("00")), 0)
    def fromBlock(template: BlockMessage) =
      BlockData(
        template.header.timestamp,
        template.body.state.blockNumber,
        PublicKey(template.sender),
        template.seqNum
      )
  }

  class InvalidBlocks[F[_]](val invalidBlocks: Ref[F, Par]) {
    def setParams(invalidBlocks: Par): F[Unit] =
      this.invalidBlocks.set(invalidBlocks)
  }

  object InvalidBlocks {
    def apply[F[_]]()(implicit F: Sync[F]): F[InvalidBlocks[F]] =
      for {
        invalidBlocks <- Ref[F].of(Par())
      } yield new InvalidBlocks[F](invalidBlocks)

    def unsafe[F[_]]()(implicit F: Sync[F]): InvalidBlocks[F] =
      new InvalidBlocks(Ref.unsafe[F, Par](Par()))
  }

  object BodyRefs {
    val STDOUT: Long             = 0L
    val STDOUT_ACK: Long         = 1L
    val STDERR: Long             = 2L
    val STDERR_ACK: Long         = 3L
    val ED25519_VERIFY: Long     = 4L
    val SHA256_HASH: Long        = 5L
    val KECCAK256_HASH: Long     = 6L
    val BLAKE2B256_HASH: Long    = 7L
    val SECP256K1_VERIFY: Long   = 9L
    val GET_BLOCK_DATA: Long     = 11L
    val GET_INVALID_BLOCKS: Long = 12L
    val REV_ADDRESS: Long        = 13L
    val DEPLOYER_ID_OPS: Long    = 14L
    val REG_OPS: Long            = 15L
    val SYS_AUTHTOKEN_OPS: Long  = 16L
  }

  def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  object FixedChannels {
    val STDOUT: Par             = byteName(0)
    val STDOUT_ACK: Par         = byteName(1)
    val STDERR: Par             = byteName(2)
    val STDERR_ACK: Par         = byteName(3)
    val ED25519_VERIFY: Par     = byteName(4)
    val SHA256_HASH: Par        = byteName(5)
    val KECCAK256_HASH: Par     = byteName(6)
    val BLAKE2B256_HASH: Par    = byteName(7)
    val SECP256K1_VERIFY: Par   = byteName(8)
    val GET_BLOCK_DATA: Par     = byteName(10)
    val GET_INVALID_BLOCKS: Par = byteName(11)
    val REV_ADDRESS: Par        = byteName(12)
    val DEPLOYER_ID_OPS: Par    = byteName(13)
    val REG_LOOKUP: Par         = byteName(14)
    val REG_INSERT_RANDOM: Par  = byteName(15)
    val REG_INSERT_SIGNED: Par  = byteName(16)
    val REG_OPS: Par            = byteName(17)
    val SYS_AUTHTOKEN_OPS: Par  = byteName(18)
  }

  def introduceSystemProcesses[F[_]: Sync: _cost: Span](
      spaces: List[RhoTuplespace[F]],
      processes: List[(Name, Arity, Remainder, BodyRef)]
  ): F[List[Option[(TaggedContinuation, Seq[ListParWithRandom])]]] =
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
        spaces.map(_.install(channels, patterns, continuation))
    }.sequence

  object SystemProcess {
    final case class Context[F[_]: Concurrent: Span](
        space: RhoTuplespace[F],
        dispatcher: RhoDispatch[F],
        blockData: Ref[F, BlockData],
        invalidBlocks: InvalidBlocks[F]
    ) {
      val systemProcesses = SystemProcesses[F](dispatcher, space)
    }

    final case class Definition[F[_]](
        urn: String,
        fixedChannel: Name,
        arity: Arity,
        bodyRef: BodyRef,
        handler: Context[F] => Seq[ListParWithRandom] => F[Unit],
        remainder: Remainder = None
    ) {
      def toDispatchTable(
          context: SystemProcess.Context[F]
      ): (BodyRef, Seq[ListParWithRandom] => F[Unit]) =
        bodyRef -> handler(context)

      def toUrnMap: (String, Par) = {
        val bundle: Par = Bundle(fixedChannel, writeFlag = true)
        urn -> bundle
      }

      def toProcDefs: (Name, Arity, Remainder, BodyRef) =
        (fixedChannel, arity, remainder, bodyRef)
    }
  }

  def stdSystemProcesses[F[_]]: Seq[SystemProcess.Definition[F]] = Seq(
    SystemProcess.Definition[F]("rho:io:stdout", FixedChannels.STDOUT, 1, BodyRefs.STDOUT, {
      ctx: SystemProcess.Context[F] =>
        ctx.systemProcesses.stdOut
    }),
    SystemProcess
      .Definition[F]("rho:io:stdoutAck", FixedChannels.STDOUT_ACK, 2, BodyRefs.STDOUT_ACK, {
        ctx: SystemProcess.Context[F] =>
          ctx.systemProcesses.stdOutAck
      }),
    SystemProcess.Definition[F]("rho:io:stderr", FixedChannels.STDERR, 1, BodyRefs.STDERR, {
      ctx: SystemProcess.Context[F] =>
        ctx.systemProcesses.stdErr
    }),
    SystemProcess
      .Definition[F]("rho:io:stderrAck", FixedChannels.STDERR_ACK, 2, BodyRefs.STDERR_ACK, {
        ctx: SystemProcess.Context[F] =>
          ctx.systemProcesses.stdErrAck
      }),
    SystemProcess.Definition[F](
      "rho:block:data",
      FixedChannels.GET_BLOCK_DATA,
      1,
      BodyRefs.GET_BLOCK_DATA, { ctx =>
        ctx.systemProcesses.getBlockData(ctx.blockData)
      }
    ),
    SystemProcess.Definition[F](
      "rho:casper:invalidBlocks",
      FixedChannels.GET_INVALID_BLOCKS,
      1,
      BodyRefs.GET_INVALID_BLOCKS, { ctx =>
        ctx.systemProcesses.invalidBlocks(ctx.invalidBlocks)
      }
    ),
    SystemProcess.Definition[F](
      "rho:rev:address",
      FixedChannels.REV_ADDRESS,
      3,
      BodyRefs.REV_ADDRESS, { ctx =>
        ctx.systemProcesses.revAddress
      }
    ),
    SystemProcess.Definition[F](
      "rho:rchain:deployerId:ops",
      FixedChannels.DEPLOYER_ID_OPS,
      3,
      BodyRefs.DEPLOYER_ID_OPS, { ctx =>
        ctx.systemProcesses.deployerIdOps
      }
    ),
    SystemProcess.Definition[F](
      "rho:registry:ops",
      FixedChannels.REG_OPS,
      3,
      BodyRefs.REG_OPS, { ctx =>
        ctx.systemProcesses.registryOps
      }
    ),
    SystemProcess.Definition[F](
      "sys:authToken:ops",
      FixedChannels.SYS_AUTHTOKEN_OPS,
      3,
      BodyRefs.SYS_AUTHTOKEN_OPS, { ctx =>
        ctx.systemProcesses.sysAuthTokenOps
      }
    )
  )

  def createWithEmptyCost[F[_]: Concurrent: Parallel: Log: Metrics: Span](
      spaceAndReplay: ISpaceAndReplay[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  ): F[Runtime[F]] =
    createWithEmptyCost_(spaceAndReplay, extraSystemProcesses)

  private def createWithEmptyCost_[F[_]: Concurrent: Parallel: Log: Metrics: Span](
      spaceAndReplay: ISpaceAndReplay[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]]
  ): F[Runtime[F]] =
    for {
      cost <- CostAccounting.emptyCost[F]
      runtime <- {
        implicit val c = cost
        create(spaceAndReplay, extraSystemProcesses)
      }
    } yield runtime

  def dispatchTableCreator[F[_]: Concurrent: Span](
      space: RhoTuplespace[F],
      dispatcher: RhoDispatch[F],
      blockData: Ref[F, BlockData],
      invalidBlocks: InvalidBlocks[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]]
  ): RhoDispatchMap[F] = {
    val systemProcesses = SystemProcesses[F](dispatcher, space)
    import BodyRefs._
    Map(
      ED25519_VERIFY   -> systemProcesses.ed25519Verify,
      SHA256_HASH      -> systemProcesses.sha256Hash,
      KECCAK256_HASH   -> systemProcesses.keccak256Hash,
      BLAKE2B256_HASH  -> systemProcesses.blake2b256Hash,
      SECP256K1_VERIFY -> systemProcesses.secp256k1Verify
    ) ++
      (stdSystemProcesses[F] ++ extraSystemProcesses)
        .map(
          _.toDispatchTable(
            SystemProcess.Context(space, dispatcher, blockData, invalidBlocks)
          )
        )
  }

  val basicProcesses: Map[String, Par] = Map[String, Par](
    "rho:crypto:secp256k1Verify"   -> Bundle(FixedChannels.SECP256K1_VERIFY, writeFlag = true),
    "rho:crypto:blake2b256Hash"    -> Bundle(FixedChannels.BLAKE2B256_HASH, writeFlag = true),
    "rho:crypto:keccak256Hash"     -> Bundle(FixedChannels.KECCAK256_HASH, writeFlag = true),
    "rho:registry:lookup"          -> Bundle(FixedChannels.REG_LOOKUP, writeFlag = true),
    "rho:registry:insertArbitrary" -> Bundle(FixedChannels.REG_INSERT_RANDOM, writeFlag = true),
    "rho:registry:insertSigned:secp256k1" -> Bundle(
      FixedChannels.REG_INSERT_SIGNED,
      writeFlag = true
    )
  )

  val basicProcessDefs: List[(Name, Arity, Remainder, BodyRef)] = {
    import BodyRefs._
    List(
      (FixedChannels.ED25519_VERIFY, 4, None, ED25519_VERIFY),
      (FixedChannels.SHA256_HASH, 2, None, SHA256_HASH),
      (FixedChannels.KECCAK256_HASH, 2, None, KECCAK256_HASH),
      (FixedChannels.BLAKE2B256_HASH, 2, None, BLAKE2B256_HASH),
      (FixedChannels.SECP256K1_VERIFY, 4, None, SECP256K1_VERIFY)
    )
  }

  def setupReducer[F[_]: Concurrent: Log: Metrics: Span](
      chargingRSpace: RhoTuplespace[F],
      blockDataRef: Ref[F, BlockData],
      invalidBlocks: InvalidBlocks[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]],
      urnMap: Map[String, Par]
  )(implicit cost: _cost[F], P: Parallel[F]): Reduce[F] = {
    lazy val replayDispatchTable: RhoDispatchMap[F] =
      dispatchTableCreator(
        chargingRSpace,
        replayDispatcher,
        blockDataRef,
        invalidBlocks,
        extraSystemProcesses
      )

    lazy val (replayDispatcher, replayReducer) =
      RholangAndScalaDispatcher.create(
        chargingRSpace,
        replayDispatchTable,
        urnMap
      )
    replayReducer
  }

  def setupMapsAndRefs[F[_]: Sync](
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  ) =
    for {
      blockDataRef  <- Ref.of(BlockData.empty)
      invalidBlocks = InvalidBlocks.unsafe[F]()
      urnMap        = basicProcesses ++ (stdSystemProcesses[F] ++ extraSystemProcesses).map(_.toUrnMap)
      procDefs = basicProcessDefs ++ (stdSystemProcesses[F] ++ extraSystemProcesses)
        .map(_.toProcDefs)
    } yield (blockDataRef, invalidBlocks, urnMap, procDefs)

  def create[F[_]: Concurrent: Parallel: _cost: Log: Metrics: Span](
      spaceAndReplay: ISpaceAndReplay[F],
      extraSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  ): F[Runtime[F]] = {
    val (space, replaySpace) = spaceAndReplay
    for {
      mapsAndRefs                                     <- setupMapsAndRefs(extraSystemProcesses)
      (blockDataRef, invalidBlocks, urnMap, procDefs) = mapsAndRefs
      (reducer, replayReducer) = {

        val replayReducer = setupReducer(
          ChargingRSpace.chargingRSpace[F](replaySpace),
          blockDataRef,
          invalidBlocks,
          extraSystemProcesses,
          urnMap
        )

        val reducer = setupReducer(
          ChargingRSpace.chargingRSpace[F](space),
          blockDataRef,
          invalidBlocks,
          extraSystemProcesses,
          urnMap
        )

        (reducer, replayReducer)
      }
      res <- introduceSystemProcesses(space :: replaySpace :: Nil, procDefs)
    } yield {
      assert(res.forall(_.isEmpty))
      new Runtime[F](
        reducer,
        replayReducer,
        space,
        replaySpace,
        _cost[F],
        blockDataRef,
        invalidBlocks
      )
    }
  }

  // This is from Nassim Taleb's "Skin in the Game"
  val bootstrapRand = Blake2b512Random(
    ("Decentralization is based on the simple notion that it is easier to macrobull***t than microbull***t. " +
      "Decentralization reduces large structural asymmetries.")
      .getBytes()
  )

  def bootstrapRegistry[F[_]: Monad](runtime: Runtime[F]): F[Unit] =
    bootstrapRegistry(runtime, runtime.reducer :: runtime.replayReducer :: Nil)

  def bootstrapRegistry[F[_]: Monad](hasCost: HasCost[F], reducers: List[Reduce[F]]): F[Unit] = {
    implicit val rand = bootstrapRand
    for {
      cost <- hasCost.cost.get
      _    <- hasCost.cost.set(Cost.UNSAFE_MAX)
      _    <- reducers.traverse_[F, Unit](_.inj(RegistryBootstrap.AST))
      _    <- hasCost.cost.set(cost)
    } yield ()
  }

  def setupRSpace[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      store: RSpaceStore[F]
  )(
      implicit scheduler: ExecutionContext
  ): F[(RhoISpace[F], RhoReplayISpace[F], RhoHistoryRepository[F])] = {
    import coop.rchain.rholang.interpreter.storage._

    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

    RSpace.createWithReplay[
      F,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ](store)
  }
}
