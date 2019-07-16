package coop.rchain.casper.engine

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore}
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.ProtocolHelper.packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.metrics.Metrics
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{Cell, Log, LogSource, Time}
import coop.rchain.catscontrib.Catscontrib, Catscontrib._
import monix.eval.Task
import monix.execution.Scheduler
import coop.rchain.models.BlockHash.BlockHash
import scala.concurrent.duration._
import scala.util.Try

/** Node in this state has already received at least one [[ApprovedBlock]] and it has created an instance
  * of [[MultiParentCasper]].
  *
  * In the future it will be possible to create checkpoint with new [[ApprovedBlock]].
  **/
object Running {

  val timeout: FiniteDuration = 30 seconds

  final case class Requested(
      timestamp: Long,
      peers: Set[PeerNode] = Set.empty,
      waitingList: List[PeerNode] = List.empty
  )

  type RequestedBlocks[F[_]] = Cell[F, Map[BlockHash, Requested]]
  object RequestedBlocks {
    def apply[F[_]: RequestedBlocks]: RequestedBlocks[F] = implicitly[RequestedBlocks[F]]
  }

  def noop[F[_]: Applicative]: F[Unit] = ().pure[F]

  private def requestForBlock[F[_]: Monad: RPConfAsk: TransportLayer: RequestedBlocks](
      peer: PeerNode,
      hash: BlockHash
  ): F[Unit] =
    for {
      conf <- RPConfAsk[F].ask
      msg = packet(
        conf.local,
        conf.networkId,
        transport.BlockRequest,
        BlockRequest(hash).toByteString
      )
      _ <- TransportLayer[F].send(peer, msg)
    } yield ()

  /**
    * This method should be called periodically to maintain liveness of the protocol
    * and keep the requested blocks list clean.
    * See spec RunningMaintainRequestedBlocksSpec for more details
    */
  def maintainRequestedBlocks[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Log: Time]
      : F[Unit] = {

    def toMap(list: List[(BlockHash, Option[Requested])]): Map[BlockHash, Requested] = {
      val filtered = list.flatMap {
        case (hash, Some(r)) => List((hash -> r))
        case (_, None)       => List.empty
      }
      Map(filtered: _*)
    }

    def tryRerequest(hash: BlockHash, requested: Requested): F[(BlockHash, Option[Requested])] =
      if (requested.waitingList.nonEmpty) {
        val nextPeer = requested.waitingList(0)
        def modifiedRequested(ts: Long) = requested.copy(
          timestamp = ts,
          waitingList = requested.waitingList.tail,
          peers = requested.peers + nextPeer
        )
        for {
          _  <- requestForBlock[F](nextPeer, hash)
          ts <- Time[F].currentMillis
        } yield ((hash -> Option(modifiedRequested(ts))))
      } else {
        val warnMessage = s"Could not retrieve requested block ${PrettyPrinter.buildString(hash)}. " +
          "Removing the request from the requested blocks list. Casper will have to re-request the block."
        Log[F].warn(warnMessage).as((hash -> none[Requested]))
      }

    RequestedBlocks[F].flatModify(requests => {
      requests.keys.toList
        .traverse(hash => {
          val requested = requests(hash)
          Time[F].currentMillis
            .map(_ - requested.timestamp > timeout.toMillis)
            .ifM(tryRerequest(hash, requested), (hash -> Option(requested)).pure[F])

        })
        .map(list => toMap(list))
    })
  }

  def handleHasBlock[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Time](
      peer: PeerNode,
      hb: HasBlock
  )(
      casperContains: BlockHash => F[Boolean]
  ): F[Unit] = {

    def addNewEntry =
      Time[F].currentMillis >>= (ts => {
        RequestedBlocks[F].modify(
          _ + (hb.hash -> Requested(timestamp = ts, peers = Set(peer)))
        )
      })

    def addToWaitingList(requested: Requested): F[Unit] =
      RequestedBlocks[F].modify { requestedBlocks =>
        requestedBlocks + (hb.hash -> requested.copy(waitingList = peer :: requested.waitingList))
      }

    casperContains(hb.hash).ifM(
      noop[F],
      RequestedBlocks[F].read >>= (_.get(hb.hash)
        .fold(requestForBlock[F](peer, hb.hash) >> addNewEntry)(addToWaitingList))
    )
  }

  def handleHasBlockRequest[F[_]: Monad: RPConfAsk: TransportLayer](
      peer: PeerNode,
      hbr: HasBlockRequest
  )(blockLookup: BlockHash => F[Boolean]): F[Unit] =
    blockLookup(hbr.hash).ifM(
      for {
        conf <- RPConfAsk[F].ask
        msg = packet(
          conf.local,
          conf.networkId,
          transport.HasBlock,
          HasBlock(hbr.hash).toByteString
        )
        _ <- TransportLayer[F].send(peer, msg)
      } yield (),
      noop[F]
    )

  def handleBlockMessage[F[_]: Monad: Log](peer: PeerNode, b: BlockMessage)(
      casper: MultiParentCasper[F]
  ): F[Unit] = {
    def handleDoppelganger(
        bm: BlockMessage,
        self: Validator
    ): F[Unit] =
      if (bm.sender == self) {
        val warnMessage =
          s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
        Log[F].warn(warnMessage)
      } else ().pure[F]

    casper
      .contains(b.blockHash)
      .ifM(
        Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again."),
        for {
          _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.")
          _ <- casper.addBlock(b, handleDoppelganger(_, _))
        } yield ()
      )
  }
}

class Running[F[_]: RPConfAsk: BlockStore: Monad: ConnectionsCell: TransportLayer: Log: Time: Running.RequestedBlocks](
    casper: MultiParentCasper[F],
    approvedBlock: ApprovedBlock,
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  import Running._

  private val blockLookup: BlockHash => F[Boolean] =
    hash => BlockStore[F].get(hash).map(_.isDefined)

  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case b: BlockMessage              => handleBlockMessage[F](peer, b)(casper)
    case br: BlockRequest             => handleBlockRequest(peer, br)
    case hbr: HasBlockRequest         => handleHasBlockRequest[F](peer, hbr)(blockLookup)
    case hb: HasBlock                 => handleHasBlock[F](peer, hb)(casper.contains)
    case fctr: ForkChoiceTipRequest   => handleForkChoiceTipRequest(peer, fctr)
    case abr: ApprovedBlockRequest    => handleApprovedBlockRequest(peer, abr)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }

  private def handleBlockRequest(peer: PeerNode, br: BlockRequest): F[Unit] =
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

  private def handleForkChoiceTipRequest(
      peer: PeerNode,
      fctr: ForkChoiceTipRequest
  ): F[Unit] =
    for {
      _     <- Log[F].info(s"Received ForkChoiceTipRequest from $peer")
      tip   <- MultiParentCasper.forkChoiceTip(casper)
      local <- RPConfAsk[F].reader(_.local)
      msg   = Blob(local, Packet(transport.BlockMessage.id, tip.toByteString))
      _     <- TransportLayer[F].stream(peer, msg)
      _     <- Log[F].info(s"Sending Block ${tip.blockHash} to $peer")
    } yield ()

  private def handleApprovedBlockRequest(
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

}
