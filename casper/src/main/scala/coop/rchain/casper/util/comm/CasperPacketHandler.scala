package coop.rchain.casper.util.comm

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Sync, Timer}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.{Capture, MonadTrans}
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.CommMessages.packet
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.shared.{Log, LogSource, Time}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object CasperPacketHandler extends CasperPacketHandlerInstances {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def apply[F[_]](implicit ev: CasperPacketHandler[F]): CasperPacketHandler[F] = ev

  def of[
      F[_]: LastApprovedBlock: Metrics: Timer: BlockStore: ConnectionsCell: NodeDiscovery: TransportLayer: ErrorHandler: RPConfAsk: SafetyOracle: Capture: Sync: Time: Log: MultiParentCasperRef](
      conf: CasperConf,
      delay: FiniteDuration,
      runtimeManager: RuntimeManager,
      toTask: F[_] => Task[_])(implicit scheduler: Scheduler): F[CasperPacketHandler[F]] =
    if (conf.approveGenesis) {
      for {
        genesis <- Genesis.fromInputFiles[F](conf.bondsFile,
                                             conf.numValidators,
                                             conf.genesisPath,
                                             conf.walletsFile,
                                             runtimeManager,
                                             conf.shardId,
                                             conf.deployTimestamp)
        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        bap         = new BlockApproverProtocol(validatorId.get, genesis, conf.requiredSigs)
        gv <- Ref.of[F, CasperPacketHandlerInternal[F]](
               new GenesisValidatorHandler(runtimeManager, validatorId.get, conf.shardId, bap))
      } yield new CasperPacketHandlerImpl[F](gv)
    } else if (conf.createGenesis) {
      for {
        genesis <- Genesis.fromInputFiles[F](conf.bondsFile,
                                             conf.numValidators,
                                             conf.genesisPath,
                                             conf.walletsFile,
                                             runtimeManager,
                                             conf.shardId,
                                             conf.deployTimestamp)
        validatorId      <- ValidatorIdentity.fromConfig[F](conf)
        bondedValidators <- Genesis.getBondedValidators[F](conf.bondsFile)
        abp <- ApproveBlockProtocol
                .of[F](genesis,
                       bondedValidators,
                       conf.requiredSigs,
                       conf.approveGenesisDuration,
                       conf.approveGenesisInterval)
                .map(protocol => {
                  toTask(protocol.run()).forkAndForget.runAsync
                  protocol
                })
        standalone <- Ref.of[F, CasperPacketHandlerInternal[F]](new StandaloneCasperHandler[F](abp))
        _ <- Sync[F].delay {
              val _ = toTask(
                StandaloneCasperHandler
                  .approveBlockInterval(conf.approveGenesisInterval,
                                        conf.shardId,
                                        runtimeManager,
                                        validatorId,
                                        standalone)).forkAndForget.runAsync
              ().pure[F]
            }
      } yield new CasperPacketHandlerImpl[F](standalone)
    } else {
      for {
        validators  <- CasperConf.parseValidatorsFile[F](conf.knownValidatorsFile)
        validatorId <- ValidatorIdentity.fromConfig[F](conf)
        bootstrap <- Ref.of[F, CasperPacketHandlerInternal[F]](
                      new BootstrapCasperHandler(runtimeManager,
                                                 conf.shardId,
                                                 validatorId,
                                                 validators))
        casperPacketHandler = new CasperPacketHandlerImpl[F](bootstrap)
        _ <- Sync[F].delay {
              implicit val ph: PacketHandler[F] = PacketHandler.pf[F](casperPacketHandler.handle)
              val rb                            = CommUtil.requestApprovedBlock[F](delay)
              toTask(rb).forkAndForget.runAsync
              ().pure[F]
            }
      } yield casperPacketHandler
    }

  trait CasperPacketHandlerInternal[F[_]] {
    def handleBlockMessage(bm: BlockMessage): F[Option[Packet]]

    def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Option[Packet]]

    def handleApprovedBlock(ab: ApprovedBlock): F[(Option[MultiParentCasper[F]], Option[Packet])]

    def handleApprovedBlockRequest(peer: PeerNode, br: ApprovedBlockRequest): F[Option[Packet]]

    def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Option[Packet]]

    def handleBlockApproval(ba: BlockApproval): F[Option[Packet]]

    def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Option[Packet]]
  }

  /** Node in this state is a genesis block validator. It will respond only to
    * [[UnapprovedBlock]] messages forwarding the logic of handling this message to
    * instance of [[BlockApproverProtocol]] class.
    *
    * When in this state node can't handle any other message type so it will return `F[None]`
    **/
  private[comm] class GenesisValidatorHandler[
      F[_]: Capture: Sync: ConnectionsCell: NodeDiscovery: TransportLayer: Log: Time: SafetyOracle: ErrorHandler: RPConfAsk: BlockStore: LastApprovedBlock](
      runtimeManager: RuntimeManager,
      validatorId: ValidatorIdentity,
      shardId: String,
      blockApprover: BlockApproverProtocol)(implicit scheduler: Scheduler)
      extends CasperPacketHandlerInternal[F] {
    private val nonePacket: F[Option[Packet]] = Monad[F].pure(None: Option[Packet])

    override def handleBlockMessage(bm: BlockMessage): F[Option[Packet]] = nonePacket
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Option[Packet]] =
      nonePacket
    override def handleApprovedBlock(
        ab: ApprovedBlock): F[(Option[MultiParentCasper[F]], Option[Packet])] =
      for {
        _ <- Log[F].info("Received ApprovedBlock message while in GenesisValidatorHandler state.")
        casperO <- onApprovedBlockTransition(ab,
                                             Set(ByteString.copyFrom(validatorId.publicKey)),
                                             runtimeManager,
                                             Some(validatorId),
                                             shardId)
        _ <- casperO.fold(Log[F].warn("MultiParentCasper instance not created."))(_ =>
              Log[F].info("MultiParentCasper instance created."))
      } yield (casperO, none[Packet])

    override def handleApprovedBlockRequest(peer: PeerNode,
                                            br: ApprovedBlockRequest): F[Option[Packet]] =
      RPConfAsk[F].reader(_.local).map(noApprovedBlockAvailable(_).some)

    override def handleBlockApproval(ba: BlockApproval): F[Option[Packet]] =
      nonePacket

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Option[Packet]] =
      blockApprover.unapprovedBlockPacketHandler(peer, ub)
    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Option[Packet]] =
      for {
        _ <- Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
      } yield none[Packet]
  }

  /** Node in this state is will send out an [[UnapprovedBlock]] message to all peers
    * and will wait for [[BlockApproval]] messages forwarding handling of those to instance of [[ApproveBlockProtocol]] class.
    * After enough [[BlockApproval]]s has been received it will create an [[ApprovedBlock]] and send it to peers.
    *
    *
    * For all other messages it will return `F[None]`.
    **/
  private[comm] class StandaloneCasperHandler[
      F[_]: Sync: Capture: ConnectionsCell: NodeDiscovery: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock](
      approveProtocol: ApproveBlockProtocol[F])(implicit scheduler: Scheduler)
      extends CasperPacketHandlerInternal[F] {

    private val nonePacket: F[Option[Packet]] = Applicative[F].pure(None: Option[Packet])

    override def handleBlockMessage(bm: BlockMessage): F[Option[Packet]] =
      nonePacket
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Option[Packet]] =
      nonePacket
    override def handleApprovedBlock(
        ab: ApprovedBlock): F[(Option[MultiParentCasper[F]], Option[Packet])] =
      nonePacket.map(none[MultiParentCasper[F]] -> _)
    override def handleApprovedBlockRequest(peer: PeerNode,
                                            br: ApprovedBlockRequest): F[Option[Packet]] =
      RPConfAsk[F].reader(_.local).map(noApprovedBlockAvailable(_).some)

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Option[Packet]] =
      nonePacket

    override def handleBlockApproval(ba: BlockApproval): F[Option[Packet]] =
      approveProtocol.addApproval(ba).map(_ => none[Packet])

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Option[Packet]] =
      for {
        _ <- Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
      } yield none[Packet]
  }

  object StandaloneCasperHandler {
    def approveBlockInterval[
        F[_]: Sync: Capture: ConnectionsCell: NodeDiscovery: BlockStore: TransportLayer: Log: Time: Timer: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: MultiParentCasperRef](
        interval: FiniteDuration,
        shardId: String,
        runtimeManager: RuntimeManager,
        validatorId: Option[ValidatorIdentity],
        capserHandlerInternal: Ref[F, CasperPacketHandlerInternal[F]])(
        implicit scheduler: Scheduler): F[Unit] =
      for {
        _                  <- Timer[F].sleep(interval)
        lastApprovedBlockO <- LastApprovedBlock[F].get
        cont <- lastApprovedBlockO match {
                 case None =>
                   approveBlockInterval[F](interval,
                                           shardId,
                                           runtimeManager,
                                           validatorId,
                                           capserHandlerInternal)
                 case Some(approvedBlock) =>
                   val blockMessage = approvedBlock.candidate.flatMap(_.block).get
                   for {
                     _           <- BlockStore[F].put(blockMessage.blockHash, blockMessage)
                     internalMap <- BlockStore[F].asMap()
                     casper = MultiParentCasper
                       .hashSetCasper[F](runtimeManager,
                                         validatorId,
                                         blockMessage,
                                         internalMap,
                                         shardId)
                     _ <- MultiParentCasperRef[F].set(casper)
                     _ <- Log[F].info(
                           "CASPER: Making a transition to ApprovedBlockRecievedHandler state.")
                     abh = new ApprovedBlockReceivedHandler[F](casper, approvedBlock)
                     _   <- capserHandlerInternal.set(abh)
                   } yield ()
               }
      } yield cont
  }

  /** Node in this state will query peers in the network with [[ApprovedBlockRequest]] message
    * and will wait for the [[ApprovedBlock]] message to arrive. Until then  it will respond with
    * `F[None]` to all other message types.
    **/
  private[comm] class BootstrapCasperHandler[
      F[_]: Sync: Capture: ConnectionsCell: NodeDiscovery: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock](
      runtimeManager: RuntimeManager,
      shardId: String,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString])(implicit scheduler: Scheduler)
      extends CasperPacketHandlerInternal[F] {
    private val nonePacket: F[Option[Packet]] = Applicative[F].pure(None: Option[Packet])

    override def handleBlockMessage(bm: BlockMessage): F[Option[Packet]] = nonePacket
    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Option[Packet]] =
      nonePacket
    override def handleApprovedBlockRequest(peer: PeerNode,
                                            br: ApprovedBlockRequest): F[Option[Packet]] =
      Option(noApprovedBlockAvailable(peer)).pure[F]

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Option[Packet]] =
      nonePacket
    override def handleBlockApproval(ba: BlockApproval): F[Option[Packet]] = nonePacket

    override def handleApprovedBlock(
        ab: ApprovedBlock): F[(Option[MultiParentCasper[F]], Option[Packet])] =
      onApprovedBlockTransition(ab, validators, runtimeManager, validatorId, shardId).map(
        _ -> none[Packet])

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Option[Packet]] =
      for {
        _ <- Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
      } yield none[Packet]

  }

  /** Node in this state has already received at least one [[ApprovedBlock]] and it has created an instance
    * of [[MultiParentCasper]].
    *
    * In the future it will be possible to create checkpoint with new [[ApprovedBlock]].
    **/
  class ApprovedBlockReceivedHandler[
      F[_]: RPConfAsk: BlockStore: Monad: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler](
      private val casper: MultiParentCasper[F],
      approvedBlock: ApprovedBlock)
      extends CasperPacketHandlerInternal[F] {

    implicit val _casper = casper

    private val nonePacket: F[Option[Packet]] = Applicative[F].pure(None: Option[Packet])

    // Possible optimization in the future
    // TODO: accept update to approved block (this will be needed for checkpointing)
    override def handleApprovedBlock(
        ab: ApprovedBlock): F[(Option[MultiParentCasper[F]], Option[Packet])] =
      nonePacket.map(none[MultiParentCasper[F]] -> _)

    override def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Option[Packet]] =
      nonePacket

    override def handleBlockApproval(b: BlockApproval): F[Option[Packet]] =
      nonePacket

    override def handleBlockMessage(b: BlockMessage): F[Option[Packet]] =
      for {
        isOldBlock <- MultiParentCasper[F].contains(b)
        _ <- if (isOldBlock) {
              Log[F].info(
                s"CASPER: Received block ${PrettyPrinter.buildString(b.blockHash)} again.")
            } else {
              handleNewBlock[F](b)
            }
      } yield none[Packet]

    override def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Option[Packet]] =
      for {
        local      <- RPConfAsk[F].reader(_.local)
        block      <- BlockStore[F].get(br.hash)
        serialized = block.map(_.toByteString)
        maybeMsg = serialized.map(serializedMessage =>
          packet(local, transport.BlockMessage, serializedMessage))
        send     <- maybeMsg.traverse(msg => TransportLayer[F].send(peer, msg))
        hash     = PrettyPrinter.buildString(br.hash)
        logIntro = s"CASPER: Received request for block $hash from $peer. "
        _ <- send match {
              case None    => Log[F].info(logIntro + "No response given since block not found.")
              case Some(_) => Log[F].info(logIntro + "Response sent.")
            }
      } yield none[Packet]

    override def handleApprovedBlockRequest(peer: PeerNode,
                                            br: ApprovedBlockRequest): F[Option[Packet]] =
      for {
        _ <- Log[F].info(s"CASPER: Received ApprovedBlockRequest from $peer")
      } yield Some(Packet(transport.ApprovedBlock.id, approvedBlock.toByteString))

    override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Option[Packet]] =
      for {
        _ <- Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
      } yield none[Packet]
  }

  class CasperPacketHandlerImpl[
      F[_]: Monad: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Log: Time: ErrorHandler: LastApprovedBlock: MultiParentCasperRef](
      private val cphI: Ref[F, CasperPacketHandlerInternal[F]])
      extends CasperPacketHandler[F] {

    override def handle(peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]] =
      Function
        .unlift(
          (p: Packet) =>
            packetToBlockRequest(p) orElse
              packetToApprovedBlock(p) orElse
              packetToApprovedBlockRequest(p) orElse
              packetToBlockMessage(p) orElse
              packetToBlockApproval(p) orElse
              packetToUnapprovedBlock(p) orElse
              packetToNoApprovedBlockAvailable(p)
        )
        .andThen {
          case br: BlockRequest =>
            for {
              cph <- cphI.get
              res <- cph.handleBlockRequest(peer, br)
            } yield res

          case ab: ApprovedBlock =>
            for {
              cph              <- cphI.get
              ret              <- cph.handleApprovedBlock(ab)
              (casper, packet) = ret
              _ <- casper match {
                    case None => ().pure[F]
                    case Some(casperInstance) =>
                      for {
                        _ <- MultiParentCasperRef[F].set(casperInstance)
                        _ <- Log[F].info(
                              "CASPER: Making a transition to ApprovedBlockRecievedHandler state.")
                        abr = new ApprovedBlockReceivedHandler(casperInstance, ab)
                        _   <- cphI.set(abr)
                      } yield ()

                  }
            } yield packet

          case abr: ApprovedBlockRequest =>
            for {
              cph <- cphI.get
              res <- cph.handleApprovedBlockRequest(peer, abr)
            } yield res

          case bm: BlockMessage =>
            for {
              cph <- cphI.get
              res <- cph.handleBlockMessage(bm)
            } yield res

          case ba: BlockApproval =>
            for {
              cph <- cphI.get
              res <- cph.handleBlockApproval(ba)
            } yield res

          case ub: UnapprovedBlock =>
            for {
              cph <- cphI.get
              res <- cph.handleUnapprovedBlock(peer, ub)
            } yield res

          case nab: NoApprovedBlockAvailable =>
            for {
              cph <- cphI.get
              res <- cph.handleNoApprovedBlockAvailable(nab)
            } yield res

        }
  }

  private def handleNewBlock[
      F[_]: Monad: MultiParentCasper: TransportLayer: Log: Time: ErrorHandler](
      b: BlockMessage): F[Unit] =
    for {
      _ <- Log[F].info(s"CASPER: Received ${PrettyPrinter.buildString(b)}.")
      _ <- MultiParentCasper[F].addBlock(b)
    } yield ()

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

  private def onApprovedBlockTransition[
      F[_]: Sync: Time: ErrorHandler: SafetyOracle: RPConfAsk: TransportLayer: Capture: ConnectionsCell: Log: BlockStore: LastApprovedBlock](
      b: ApprovedBlock,
      validators: Set[ByteString],
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      shardId: String)(implicit scheduler: Scheduler): F[Option[MultiParentCasper[F]]] =
    for {
      isValid <- Validate.approvedBlock[F](b, validators)
      casper <- if (isValid) {
                 for {
                   _            <- Log[F].info("CASPER: Valid ApprovedBlock received!")
                   blockMessage = b.candidate.flatMap(_.block).get
                   _            <- BlockStore[F].put(blockMessage.blockHash, blockMessage)
                   internalMap  <- BlockStore[F].asMap()
                   _            <- LastApprovedBlock[F].set(b)
                   casper = MultiParentCasper
                     .hashSetCasper[F](runtimeManager,
                                       validatorId,
                                       blockMessage,
                                       internalMap,
                                       shardId)
                 } yield Option(casper)
               } else
                 Log[F]
                   .info("CASPER: Invalid ApprovedBlock received; refusing to add.")
                   .map(_ => none[MultiParentCasper[F]])
    } yield casper

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: CasperPacketHandler[F]): CasperPacketHandler[T[F, ?]] =
    new CasperPacketHandler[T[F, ?]] {
      override def handle(peer: PeerNode): PartialFunction[Packet, T[F, Option[Packet]]] =
        PartialFunction { (p: Packet) =>
          C.handle(peer)(p).liftM[T]
        }
    }

  private def noApprovedBlockAvailable(peer: PeerNode): Packet =
    Packet(transport.NoApprovedBlockAvailable.id,
           NoApprovedBlockAvailable("NoApprovedBlockAvailable", peer.toString).toByteString)

}

trait CasperPacketHandler[F[_]] {
  def handle(peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]]
}

abstract class CasperPacketHandlerInstances {
  implicit def eitherTCasperPacketHandler[E, F[_]: Monad: CasperPacketHandler[?[_]]]
    : CasperPacketHandler[EitherT[F, E, ?]] = CasperPacketHandler.forTrans[F, EitherT[?[_], E, ?]]
}
