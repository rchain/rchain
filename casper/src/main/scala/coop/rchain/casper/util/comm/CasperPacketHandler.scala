package coop.rchain.casper.util.comm

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

class CasperInit[F[_]](
    val conf: CasperConf,
    val delay: FiniteDuration,
    val runtimeManager: RuntimeManager[F]
)

object CasperPacketHandler extends CasperPacketHandlerInstances {
  implicit private val logSource: LogSource = LogSource(this.getClass)

  def apply[F[_]](implicit ev: CasperPacketHandler[F]): CasperPacketHandler[F] = ev

  def of[F[_]: LastApprovedBlock: Metrics: BlockStore: ConnectionsCell: NodeDiscovery: TransportLayer: ErrorHandler: RPConfAsk: SafetyOracle: Sync: Concurrent: Time: Log: MultiParentCasperRef: BlockDagStorage](
      init: CasperInit[F],
      toTask: F[_] => Task[_]
  )(implicit scheduler: Scheduler): F[CasperPacketHandler[F]] =
    BlockStore[F].getApprovedBlock map {
      case Some(approvedBlock) =>
        val msg    = "Found ApprovedBlock in storage, reconnecting to existing network"
        val action = connectToExistingNetwork[F](approvedBlock, init)
        (msg, action)
      case None if (init.conf.approveGenesis) =>
        val msg    = "ApprovedBlock not found in storage, taking part in ceremony as genesis validator"
        val action = connectAsGenesisValidator[F](init)
        (msg, action)
      case None if (init.conf.createGenesis) =>
        val msg =
          "ApprovedBlock not found in storage, taking part in ceremony as ceremony master"
        val action = initBootstrap[F](init, toTask)
        (msg, action)
      case None =>
        val msg    = "ApprovedBlock not found in storage, connecting to existing network"
        val action = connectAndQueryApprovedBlock[F](init)
        (msg, action)
    } >>= {
      case (msg, action) => Log[F].info(msg) >> action
    }

  def connectToExistingNetwork[F[_]: LastApprovedBlock: Metrics: BlockStore: ConnectionsCell: NodeDiscovery: TransportLayer: ErrorHandler: RPConfAsk: SafetyOracle: Concurrent: Time: Log: MultiParentCasperRef: BlockDagStorage](
      approvedBlock: ApprovedBlock,
      init: CasperInit[F]
  ): F[CasperPacketHandler[F]] =
    for {
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      genesis     = approvedBlock.candidate.flatMap(_.block).get
      casper <- MultiParentCasper.hashSetCasper[F](
                 init.runtimeManager,
                 validatorId,
                 genesis,
                 init.conf.shardId
               )
      _                   <- MultiParentCasperRef[F].set(casper)
      _                   <- Log[F].info("Making a transition to ApprovedBlockReceivedHandler state.")
      abh                 = new ApprovedBlockReceivedHandler[F](casper, approvedBlock)
      validator           <- Ref.of[F, CasperPacketHandlerInternal[F]](abh)
      casperPacketHandler = new CasperPacketHandlerImpl[F](validator)
    } yield casperPacketHandler

  def connectAsGenesisValidator[F[_]: Monad: Sync: Metrics: LastApprovedBlock: ErrorHandler: Time: Concurrent: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: SafetyOracle: BlockDagStorage](
      init: CasperInit[F]
  ): F[CasperPacketHandler[F]] =
    for {
      walletsFile <- Genesis
                      .toFile[F](
                        init.conf.walletsFile,
                        init.conf.genesisPath.resolve("wallets.txt")
                      )
      wallets   <- Genesis.getWallets[F](walletsFile, init.conf.walletsFile)
      timestamp <- init.conf.deployTimestamp.fold(Time[F].currentMillis)(_.pure[F])
      bondsFile <- Genesis
                    .toFile[F](init.conf.bondsFile, init.conf.genesisPath.resolve("bonds.txt"))
      bonds <- Genesis
                .getBonds[F](bondsFile, init.conf.numValidators, init.conf.genesisPath)
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bap = new BlockApproverProtocol(
        validatorId.get,
        timestamp,
        bonds,
        wallets,
        init.conf.minimumBond,
        init.conf.maximumBond,
        init.conf.hasFaucet,
        init.conf.requiredSigs
      )
      gv <- Ref.of[F, CasperPacketHandlerInternal[F]](
             new GenesisValidatorHandler(
               init.runtimeManager,
               validatorId.get,
               init.conf.shardId,
               bap
             )
           )
    } yield new CasperPacketHandlerImpl[F](gv)

  def initBootstrap[F[_]: Monad: Sync: LastApprovedBlock: ErrorHandler: Time: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Concurrent: Metrics: SafetyOracle: BlockDagStorage](
      init: CasperInit[F],
      toTask: F[_] => Task[_]
  )(implicit scheduler: Scheduler): F[CasperPacketHandler[F]] =
    for {
      genesis <- Genesis.fromInputFiles[F](
                  init.conf.bondsFile,
                  init.conf.numValidators,
                  init.conf.genesisPath,
                  init.conf.walletsFile,
                  init.conf.minimumBond,
                  init.conf.maximumBond,
                  init.conf.hasFaucet,
                  init.runtimeManager,
                  init.conf.shardId,
                  init.conf.deployTimestamp
                )
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bondedValidators = genesis.body
        .flatMap(_.state.map(_.bonds.map(_.validator).toSet))
        .getOrElse(Set.empty)
      abp <- ApproveBlockProtocol
              .of[F](
                genesis,
                bondedValidators,
                init.conf.requiredSigs,
                init.conf.approveGenesisDuration,
                init.conf.approveGenesisInterval
              )
      standalone <- Ref.of[F, CasperPacketHandlerInternal[F]](
                     new StandaloneCasperHandler[F](abp)
                   )
      // TODO OMG Fix, use Concurrent+!11
      _ <- Sync[F].delay {
            val _ = toTask(
              StandaloneCasperHandler
                .approveBlockInterval(
                  init.conf.approveGenesisInterval,
                  init.conf.shardId,
                  init.runtimeManager,
                  validatorId,
                  standalone
                )
            ).forkAndForget.runToFuture
            ().pure[F]
          }
    } yield new CasperPacketHandlerImpl[F](standalone)

  def connectAndQueryApprovedBlock[F[_]: Monad: Sync: LastApprovedBlock: ErrorHandler: Time: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Metrics: Concurrent: SafetyOracle: BlockDagStorage](
      init: CasperInit[F]
  ): F[CasperPacketHandler[F]] =
    for {
      validators  <- CasperConf.parseValidatorsFile[F](init.conf.knownValidatorsFile)
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bootstrap <- Ref.of[F, CasperPacketHandlerInternal[F]] {
                    new BootstrapCasperHandler(
                      init.runtimeManager,
                      init.conf.shardId,
                      validatorId,
                      validators,
                      CommUtil.requestApprovedBlock[F](init.delay)
                    )
                  }
      casperPacketHandler = new CasperPacketHandlerImpl[F](bootstrap)
    } yield casperPacketHandler

  trait CasperPacketHandlerInternal[F[_]] {
    def init: F[Unit]

    def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit]

    def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit]

    def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit]

    def handleApprovedBlock(ab: ApprovedBlock): F[Option[MultiParentCasper[F]]]

    def handleApprovedBlockRequest(peer: PeerNode, br: ApprovedBlockRequest): F[Unit]

    def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit]

    def handleBlockApproval(ba: BlockApproval): F[Unit]

    def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit]
  }

  /** Node in this state is a genesis block validator. It will respond only to
    * [[UnapprovedBlock]] messages forwarding the logic of handling this message to
    * instance of [[BlockApproverProtocol]] class.
    *
    * When in this state node can't handle any other message type so it will return `F[None]`
    **/
  private[comm] class GenesisValidatorHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: TransportLayer: Log: Time: SafetyOracle: ErrorHandler: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage](
      runtimeManager: RuntimeManager[F],
      validatorId: ValidatorIdentity,
      shardId: String,
      blockApprover: BlockApproverProtocol
  ) extends CasperPacketHandlerInternal[F] {
    private val noop: F[Unit] = Applicative[F].unit

    override def init: F[Unit] = noop

    override def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit] =
      noop
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit] =
      noop
    override def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit] =
      noop
    override def handleApprovedBlock(
        ab: ApprovedBlock
    ): F[Option[MultiParentCasper[F]]] =
      for {
        _ <- Log[F].info("Received ApprovedBlock message while in GenesisValidatorHandler state.")
        casperO <- onApprovedBlockTransition(
                    ab,
                    Set(ByteString.copyFrom(validatorId.publicKey.bytes)),
                    runtimeManager,
                    Some(validatorId),
                    shardId
                  )
        _ <- casperO.fold(Log[F].warn("MultiParentCasper instance not created."))(
              _ => Log[F].info("MultiParentCasper instance created.")
            )
      } yield casperO

    override def handleApprovedBlockRequest(
        peer: PeerNode,
        br: ApprovedBlockRequest
    ): F[Unit] =
      sendNoApprovedBlockAvailable(peer, br.identifier)

    override def handleBlockApproval(ba: BlockApproval): F[Unit] =
      noop

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit] =
      blockApprover.unapprovedBlockPacketHandler(peer, ub, runtimeManager)

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
      Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
  }

  /** Node in this state is will send out an [[UnapprovedBlock]] message to all peers
    * and will wait for [[BlockApproval]] messages forwarding handling of those to instance of [[ApproveBlockProtocol]] class.
    * After enough [[BlockApproval]]s has been received it will create an [[ApprovedBlock]] and send it to peers.
    *
    *
    * For all other messages it will return `F[None]`.
    **/
  private[comm] class StandaloneCasperHandler[F[_]: Sync: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock](
      approveProtocol: ApproveBlockProtocol[F]
  ) extends CasperPacketHandlerInternal[F] {

    private val noop: F[Unit] = Applicative[F].unit

    override def init: F[Unit] = approveProtocol.run()

    override def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit] =
      noop
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit] =
      noop
    override def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit] =
      noop
    override def handleApprovedBlock(
        ab: ApprovedBlock
    ): F[Option[MultiParentCasper[F]]] =
      none[MultiParentCasper[F]].pure[F]
    override def handleApprovedBlockRequest(
        peer: PeerNode,
        br: ApprovedBlockRequest
    ): F[Unit] =
      sendNoApprovedBlockAvailable(peer, br.identifier)

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit] =
      noop

    override def handleBlockApproval(ba: BlockApproval): F[Unit] =
      approveProtocol.addApproval(ba)

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
      Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
  }

  object StandaloneCasperHandler {
    def approveBlockInterval[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: MultiParentCasperRef: BlockDagStorage](
        interval: FiniteDuration,
        shardId: String,
        runtimeManager: RuntimeManager[F],
        validatorId: Option[ValidatorIdentity],
        capserHandlerInternal: Ref[F, CasperPacketHandlerInternal[F]]
    ): F[Unit] =
      for {
        _                  <- Time[F].sleep(interval)
        lastApprovedBlockO <- LastApprovedBlock[F].get
        cont <- lastApprovedBlockO match {
                 case None =>
                   approveBlockInterval[F](
                     interval,
                     shardId,
                     runtimeManager,
                     validatorId,
                     capserHandlerInternal
                   )
                 case Some(approvedBlock) =>
                   val genesis = approvedBlock.candidate.flatMap(_.block).get
                   for {
                     _ <- insertIntoBlockAndDagStore[F](genesis, approvedBlock)
                     casper <- MultiParentCasper.hashSetCasper[F](
                                runtimeManager,
                                validatorId,
                                genesis,
                                shardId
                              )
                     _   <- MultiParentCasperRef[F].set(casper)
                     _   <- Log[F].info("Making a transition to ApprovedBlockReceivedHandler state.")
                     abh = new ApprovedBlockReceivedHandler[F](casper, approvedBlock)
                     _   <- capserHandlerInternal.set(abh)
                     _   <- CommUtil.sendForkChoiceTipRequest[F]
                   } yield ()
               }
      } yield cont
  }

  /** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
    * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
    * `F[None]` to all other message types.
    **/
  private[comm] class BootstrapCasperHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: BlockDagStorage](
      runtimeManager: RuntimeManager[F],
      shardId: String,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString],
      theInit: F[Unit]
  ) extends CasperPacketHandlerInternal[F] {
    private val noop: F[Unit] = Applicative[F].unit

    override def init: F[Unit] = theInit

    override def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit] =
      noop
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit] =
      noop
    override def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit] =
      noop
    override def handleApprovedBlockRequest(
        peer: PeerNode,
        br: ApprovedBlockRequest
    ): F[Unit] =
      sendNoApprovedBlockAvailable(peer, br.identifier)

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit] =
      noop
    override def handleBlockApproval(ba: BlockApproval): F[Unit] = noop

    override def handleApprovedBlock(
        ab: ApprovedBlock
    ): F[Option[MultiParentCasper[F]]] =
      onApprovedBlockTransition(
        ab,
        validators,
        runtimeManager,
        validatorId,
        shardId
      )

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
      Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")

  }

  /** Node in this state has already received at least one [[ApprovedBlock]] and it has created an instance
    * of [[MultiParentCasper]].
    *
    * In the future it will be possible to create checkpoint with new [[ApprovedBlock]].
    **/
  class ApprovedBlockReceivedHandler[F[_]: RPConfAsk: BlockStore: Monad: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler](
      private val casper: MultiParentCasper[F],
      approvedBlock: ApprovedBlock
  ) extends CasperPacketHandlerInternal[F] {

    implicit val _casper = casper

    private val noop: F[Unit] = Applicative[F].unit

    override def init: F[Unit] = noop

    // Possible optimization in the future
    // TODO: accept update to approved block (this will be needed for checkpointing)
    override def handleApprovedBlock(
        ab: ApprovedBlock
    ): F[Option[MultiParentCasper[F]]] =
      none[MultiParentCasper[F]].pure[F]

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit] =
      noop

    override def handleBlockApproval(b: BlockApproval): F[Unit] =
      noop

    override def handleBlockMessage(peer: PeerNode, b: BlockMessage): F[Unit] =
      for {
        isOldBlock <- MultiParentCasper[F].contains(b)
        _ <- if (isOldBlock) {
              Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again.")
            } else {
              handleNewBlock[F](peer, b)
            }
      } yield ()

    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit] =
      for {
        local      <- RPConfAsk[F].reader(_.local)
        block      <- BlockStore[F].get(br.hash) // TODO: Refactor
        serialized = block.map(_.toByteString)
        maybeMsg = serialized.map(
          serializedMessage => Blob(local, Packet(transport.BlockMessage.id, serializedMessage))
        )
        _        <- maybeMsg.traverse(msg => TransportLayer[F].stream(peer, msg))
        hash     = PrettyPrinter.buildString(br.hash)
        logIntro = s"Received request for block $hash from $peer."
        _ <- block match {
              case None    => Log[F].info(logIntro + "No response given since block not found.")
              case Some(_) => Log[F].info(logIntro + "Response sent.")
            }
      } yield ()

    override def handleForkChoiceTipRequest(
        peer: PeerNode,
        fctr: ForkChoiceTipRequest
    ): F[Unit] =
      for {
        _     <- Log[F].info(s"Received ForkChoiceTipRequest from $peer")
        tip   <- MultiParentCasper.forkChoiceTip
        local <- RPConfAsk[F].reader(_.local)
        msg   = Blob(local, Packet(transport.BlockMessage.id, tip.toByteString))
        _     <- TransportLayer[F].stream(peer, msg)
        _     <- Log[F].info(s"Sending Block ${tip.blockHash} to $peer")
      } yield ()

    override def handleApprovedBlockRequest(
        peer: PeerNode,
        br: ApprovedBlockRequest
    ): F[Unit] =
      for {
        local <- RPConfAsk[F].reader(_.local)
        _     <- Log[F].info(s"Received ApprovedBlockRequest from $peer")
        msg   = Blob(local, Packet(transport.ApprovedBlock.id, approvedBlock.toByteString))
        _     <- TransportLayer[F].stream(peer, msg)
        _     <- Log[F].info(s"Sending ApprovedBlock to $peer")
      } yield ()

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
      Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
  }

  class CasperPacketHandlerImpl[F[_]: Monad: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: LastApprovedBlock: MultiParentCasperRef](
      private val cphI: Ref[F, CasperPacketHandlerInternal[F]]
  ) extends CasperPacketHandler[F] {

    override def init: F[Unit] =
      Log[F].info("Executing init of CasperPacketHandlerImpl") >> cphI.get >>= (_.init)

    override def handle(peer: PeerNode): PartialFunction[Packet, F[Unit]] =
      Function
        .unlift(
          (p: Packet) =>
            packetToBlockRequest(p) orElse
              packetToForkChoiceTipRequest(p) orElse
              packetToApprovedBlock(p) orElse
              packetToApprovedBlockRequest(p) orElse
              packetToBlockMessage(p) orElse
              packetToBlockApproval(p) orElse
              packetToUnapprovedBlock(p) orElse
              packetToNoApprovedBlockAvailable(p)
        )
        .andThen {
          case br: BlockRequest           => cphI.get >>= (_.handleBlockRequest(peer, br))
          case fctr: ForkChoiceTipRequest => cphI.get >>= (_.handleForkChoiceTipRequest(peer, fctr))
          case ab: ApprovedBlock =>
            cphI.get >>= (_.handleApprovedBlock(ab)) >>= {
              case None => ().pure[F]
              case Some(casperInstance) =>
                for {
                  _ <- MultiParentCasperRef[F].set(casperInstance)
                  _ <- Log[F].info(
                        "Making a transition to ApprovedBlockReceivedHandler state."
                      )
                  abr = new ApprovedBlockReceivedHandler(casperInstance, ab)
                  _   <- cphI.set(abr)
                  _   <- CommUtil.sendForkChoiceTipRequest[F]
                } yield ()
            }
          case abr: ApprovedBlockRequest     => cphI.get >>= (_.handleApprovedBlockRequest(peer, abr))
          case bm: BlockMessage              => cphI.get >>= (_.handleBlockMessage(peer, bm))
          case ba: BlockApproval             => cphI.get >>= (_.handleBlockApproval(ba))
          case ub: UnapprovedBlock           => cphI.get >>= (_.handleUnapprovedBlock(peer, ub))
          case nab: NoApprovedBlockAvailable => cphI.get >>= (_.handleNoApprovedBlockAvailable(nab))
        }
  }

  private def handleNewBlock[F[_]: Monad: MultiParentCasper: TransportLayer: Log: Time: ErrorHandler](
      peer: PeerNode,
      b: BlockMessage
  ): F[Unit] =
    for {
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.")
      _ <- MultiParentCasper[F].addBlock(b, handleDoppelganger[F](peer, _, _))
    } yield ()

  private def handleDoppelganger[F[_]: Monad: Log](
      peer: PeerNode,
      b: BlockMessage,
      self: Validator
  ): F[Unit] =
    if (b.sender == self) {
      Log[F].warn(
        s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
      )
    } else {
      ().pure[F]
    }

  private def packetToBlockMessage(msg: Packet): Option[BlockMessage] =
    if (msg.typeId == transport.BlockMessage.id)
      Try(BlockMessage.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToApprovedBlock(msg: Packet): Option[ApprovedBlock] =
    if (msg.typeId == transport.ApprovedBlock.id)
      Try(ApprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToApprovedBlockRequest(msg: Packet): Option[ApprovedBlockRequest] =
    if (msg.typeId == transport.ApprovedBlockRequest.id)
      Try(ApprovedBlockRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToBlockRequest(msg: Packet): Option[BlockRequest] =
    if (msg.typeId == transport.BlockRequest.id)
      Try(BlockRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToForkChoiceTipRequest(msg: Packet): Option[ForkChoiceTipRequest] =
    if (msg.typeId == transport.ForkChoiceTipRequest.id)
      Try(ForkChoiceTipRequest.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToBlockApproval(msg: Packet): Option[BlockApproval] =
    if (msg.typeId == transport.BlockApproval.id)
      Try(BlockApproval.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    if (msg.typeId == transport.UnapprovedBlock.id)
      Try(UnapprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def packetToNoApprovedBlockAvailable(msg: Packet): Option[NoApprovedBlockAvailable] =
    if (msg.typeId == transport.NoApprovedBlockAvailable.id)
      Try(NoApprovedBlockAvailable.parseFrom(msg.content.toByteArray)).toOption
    else None

  private def onApprovedBlockTransition[F[_]: Sync: Metrics: Concurrent: Time: ErrorHandler: SafetyOracle: RPConfAsk: TransportLayer: ConnectionsCell: Log: BlockStore: LastApprovedBlock: BlockDagStorage](
      b: ApprovedBlock,
      validators: Set[ByteString],
      runtimeManager: RuntimeManager[F],
      validatorId: Option[ValidatorIdentity],
      shardId: String
  ): F[Option[MultiParentCasper[F]]] =
    for {
      isValid <- Validate.approvedBlock[F](b, validators)
      casper <- if (isValid) {
                 for {
                   _       <- Log[F].info("Valid ApprovedBlock received!")
                   genesis = b.candidate.flatMap(_.block).get
                   _       <- insertIntoBlockAndDagStore[F](genesis, b)
                   _       <- LastApprovedBlock[F].set(b)
                   casper <- MultiParentCasper
                              .hashSetCasper[F](
                                runtimeManager,
                                validatorId,
                                genesis,
                                shardId
                              )
                 } yield Option(casper)
               } else
                 Log[F]
                   .info("Invalid ApprovedBlock received; refusing to add.")
                   .map(_ => none[MultiParentCasper[F]])
    } yield casper

  /*
   * Note the ordering of the insertions is important.
   * We always want the block dag store to be a subset of the block store.
   */
  private def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: ErrorHandler: TransportLayer: ConnectionsCell: Log: BlockStore: BlockDagStorage](
      genesis: BlockMessage,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    for {
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insert(genesis, genesis, invalid = false)
      _ <- BlockStore[F].putApprovedBlock(approvedBlock)
    } yield ()

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: CasperPacketHandler[F]
  ): CasperPacketHandler[T[F, ?]] =
    new CasperPacketHandler[T[F, ?]] {
      override def init: T[F, Unit] = C.init.liftM[T]

      override def handle(peer: PeerNode): PartialFunction[Packet, T[F, Unit]] = {
        case (p: Packet) =>
          C.handle(peer)(p).liftM[T]
      }
    }

  private def sendNoApprovedBlockAvailable[F[_]: RPConfAsk: TransportLayer: Monad](
      peer: PeerNode,
      identifier: String
  ): F[Unit] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      //TODO remove NoApprovedBlockAvailable.nodeIdentifier, use `sender` provided by TransportLayer
      msg = Blob(local, noApprovedBlockAvailable(local, identifier))
      _   <- TransportLayer[F].stream(peer, msg)
    } yield ()

  private def noApprovedBlockAvailable(peer: PeerNode, identifier: String): Packet = Packet(
    transport.NoApprovedBlockAvailable.id,
    NoApprovedBlockAvailable(identifier, peer.toString).toByteString
  )

}

trait CasperPacketHandler[F[_]] {
  def handle(peer: PeerNode): PartialFunction[Packet, F[Unit]]
  def init: F[Unit]
}

abstract class CasperPacketHandlerInstances {
  implicit def eitherTCasperPacketHandler[E, F[_]: Monad: CasperPacketHandler[?[_]]]
      : CasperPacketHandler[EitherT[F, E, ?]] = CasperPacketHandler.forTrans[F, EitherT[?[_], E, ?]]
}
