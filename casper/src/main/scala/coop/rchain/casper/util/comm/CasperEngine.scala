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

trait CasperEngine[F[_]] {

  def applicative: Applicative[F]

  val noop: F[Unit]                                                                   = applicative.unit
  def init: F[Unit]                                                                   = noop
  def handleBlockMessage(peer: PeerNode, bm: BlockMessage): F[Unit]                   = noop
  def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit]                   = noop
  def handleForkChoiceTipRequest(peer: PeerNode, fctr: ForkChoiceTipRequest): F[Unit] = noop
  def handleApprovedBlock(ab: ApprovedBlock): F[Option[MultiParentCasper[F]]] =
    applicative.pure(none[MultiParentCasper[F]])
  def handleApprovedBlockRequest(peer: PeerNode, br: ApprovedBlockRequest): F[Unit] = noop
  def handleUnapprovedBlock(peer: PeerNode, ub: UnapprovedBlock): F[Unit]           = noop
  def handleBlockApproval(ba: BlockApproval): F[Unit]                               = noop
  def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit]         = noop
}

object CasperEngine {
  def connectAndQueryApprovedBlock[F[_]: Monad: Sync: LastApprovedBlock: ErrorHandler: Time: MultiParentCasperRef: Log: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Metrics: Concurrent: SafetyOracle: BlockDagStorage](
      init: CasperInit[F]
  ): F[CasperPacketHandler[F]] =
    for {
      validators  <- CasperConf.parseValidatorsFile[F](init.conf.knownValidatorsFile)
      validatorId <- ValidatorIdentity.fromConfig[F](init.conf)
      bootstrap <- Ref.of[F, CasperEngine[F]] {
                    new BootstrapCasperHandler(
                      init.runtimeManager,
                      init.conf.shardId,
                      validatorId,
                      validators,
                      CommUtil.requestApprovedBlock[F]
                    )
                  }
      casperPacketHandler = new CasperPacketHandler[F](bootstrap)
    } yield casperPacketHandler

  /*
   * Note the ordering of the insertions is important.
   * We always want the block dag store to be a subset of the block store.
   */
  def insertIntoBlockAndDagStore[F[_]: Sync: Concurrent: ErrorHandler: TransportLayer: ConnectionsCell: Log: BlockStore: BlockDagStorage](
      genesis: BlockMessage,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    for {
      _ <- BlockStore[F].put(genesis.blockHash, genesis)
      _ <- BlockDagStorage[F].insert(genesis, genesis, invalid = false)
      _ <- BlockStore[F].putApprovedBlock(approvedBlock)
    } yield ()

  private def noApprovedBlockAvailable(peer: PeerNode, identifier: String): Packet = Packet(
    transport.NoApprovedBlockAvailable.id,
    NoApprovedBlockAvailable(identifier, peer.toString).toByteString
  )

  def sendNoApprovedBlockAvailable[F[_]: RPConfAsk: TransportLayer: Monad](
      peer: PeerNode,
      identifier: String
  ): F[Unit] =
    for {
      local <- RPConfAsk[F].reader(_.local)
      //TODO remove NoApprovedBlockAvailable.nodeIdentifier, use `sender` provided by TransportLayer
      msg = Blob(local, noApprovedBlockAvailable(local, identifier))
      _   <- TransportLayer[F].stream(peer, msg)
    } yield ()

  def onApprovedBlockTransition[F[_]: Sync: Metrics: Concurrent: Time: ErrorHandler: SafetyOracle: RPConfAsk: TransportLayer: ConnectionsCell: Log: BlockStore: LastApprovedBlock: BlockDagStorage](
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
}

/** Node in this state is a genesis block validator. It will respond only to
  * [[UnapprovedBlock]] messages forwarding the logic of handling this message to
  * instance of [[BlockApproverProtocol]] class.
  *
  * When in this state node can't handle any other message type so it will return `F[None]`
    **/
class GenesisValidatorHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: TransportLayer: Log: Time: SafetyOracle: ErrorHandler: RPConfAsk: BlockStore: LastApprovedBlock: BlockDagStorage](
    runtimeManager: RuntimeManager[F],
    validatorId: ValidatorIdentity,
    shardId: String,
    blockApprover: BlockApproverProtocol
) extends CasperEngine[F] {
  import CasperEngine._
  def applicative: Applicative[F] = Applicative[F]

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
class StandaloneCasperHandler[F[_]: Sync: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock](
    approveProtocol: ApproveBlockProtocol[F]
) extends CasperEngine[F] {
  import CasperEngine._
  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = approveProtocol.run()

  override def handleApprovedBlock(
      ab: ApprovedBlock
  ): F[Option[MultiParentCasper[F]]] =
    none[MultiParentCasper[F]].pure[F]
  override def handleApprovedBlockRequest(
      peer: PeerNode,
      br: ApprovedBlockRequest
  ): F[Unit] =
    sendNoApprovedBlockAvailable(peer, br.identifier)

  override def handleBlockApproval(ba: BlockApproval): F[Unit] =
    approveProtocol.addApproval(ba)

  override def handleNoApprovedBlockAvailable(na: NoApprovedBlockAvailable): F[Unit] =
    Log[F].info(s"No approved block available on node ${na.nodeIdentifer}")
}

object StandaloneCasperHandler {
  import CasperEngine._
  def approveBlockInterval[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: MultiParentCasperRef: BlockDagStorage](
      interval: FiniteDuration,
      shardId: String,
      runtimeManager: RuntimeManager[F],
      validatorId: Option[ValidatorIdentity],
      capserHandlerInternal: Ref[F, CasperEngine[F]]
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
class BootstrapCasperHandler[F[_]: Sync: Metrics: Concurrent: ConnectionsCell: BlockStore: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: RPConfAsk: LastApprovedBlock: BlockDagStorage](
    runtimeManager: RuntimeManager[F],
    shardId: String,
    validatorId: Option[ValidatorIdentity],
    validators: Set[ByteString],
    theInit: F[Unit]
) extends CasperEngine[F] {
  import CasperEngine._
  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = theInit

  override def handleApprovedBlockRequest(
      peer: PeerNode,
      br: ApprovedBlockRequest
  ): F[Unit] = sendNoApprovedBlockAvailable(peer, br.identifier)

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
) extends CasperEngine[F] {

  implicit val _casper            = casper
  def applicative: Applicative[F] = Applicative[F]

  private def handleDoppelganger(
      peer: PeerNode,
      b: BlockMessage,
      self: Validator
  ): F[Unit] =
    if (b.sender == self) {
      Log[F].warn(
        s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
      )
    } else ().pure[F]

  private def handleNewBlock(
      peer: PeerNode,
      b: BlockMessage
  ): F[Unit] =
    for {
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.")
      _ <- MultiParentCasper[F].addBlock(b, handleDoppelganger(peer, _, _))
    } yield ()

  override def handleBlockMessage(peer: PeerNode, b: BlockMessage): F[Unit] =
    for {
      isOldBlock <- MultiParentCasper[F].contains(b)
      _ <- if (isOldBlock)
            Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again.")
          else
            handleNewBlock(peer, b)

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
