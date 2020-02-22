package coop.rchain.casper.engine

import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.protocol
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{Cell, Log, Time}

import scala.concurrent.duration._

object Running {

  implicit private[this] val BlockRequesterMetricsSource =
    Metrics.Source(CasperMetricsSource, "block-requester")

  final case class Requested(
      timestamp: Long,
      peers: Set[PeerNode] = Set.empty,
      waitingList: List[PeerNode] = List.empty
  )

  type RequestedBlocks[F[_]] = Cell[F, Map[BlockHash, Requested]]
  object RequestedBlocks {
    def apply[F[_]: RequestedBlocks]: RequestedBlocks[F] = implicitly[RequestedBlocks[F]]
    def put[F[_]: RequestedBlocks](hash: BlockHash, requested: Requested) =
      RequestedBlocks[F].modify(_ + (hash -> requested))
    def remove[F[_]: RequestedBlocks](hash: BlockHash) = RequestedBlocks[F].modify(_ - hash)
    def get[F[_]: RequestedBlocks](hash: BlockHash)    = RequestedBlocks[F].reads(_.get(hash))
    def contains[F[_]: RequestedBlocks](hash: BlockHash) =
      RequestedBlocks[F].reads(_.contains(hash))
  }

  private def sendToPeer[F[_]: Monad: RPConfAsk: TransportLayer](peer: PeerNode)(packet: Packet) =
    RPConfAsk[F].ask
      .map(conf => protocol(conf.local, conf.networkId).withPacket(packet))
      .flatMap(msg => TransportLayer[F].send(peer, msg))

  private def streamToPeer[F[_]: Monad: RPConfAsk: TransportLayer](peer: PeerNode)(packet: Packet) =
    RPConfAsk[F].ask
      .map(conf => Blob(conf.local, packet))
      .flatMap(blob => TransportLayer[F].stream(peer, blob))

  private def requestForBlock[F[_]: Monad: RPConfAsk: TransportLayer: RequestedBlocks](
      peer: PeerNode,
      hash: BlockHash
  ) = sendToPeer(peer)(ToPacket(BlockRequestProto(hash)))

  private def requestForNewBlock[F[_]: Monad: RPConfAsk: TransportLayer: RequestedBlocks: Log: Time](
      peer: PeerNode,
      blockHash: BlockHash
  ) = requestForBlock(peer, blockHash) >> addNewEntry(blockHash, Some(peer))

  /**
    * This method should be called periodically to maintain liveness of the protocol
    * and keep the requested blocks list clean.
    * See spec RunningMaintainRequestedBlocksSpec for more details
    */
  def maintainRequestedBlocks[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Log: Time: Metrics](
      timeout: Int
  ): F[Unit] = {

    def toMap(list: List[(BlockHash, Option[Requested])]): Map[BlockHash, Requested] = {
      val filtered = list.flatMap {
        case (hash, Some(r)) => List((hash -> r))
        case (_, None)       => List.empty
      }
      Map(filtered: _*)
    }

    def tryRerequest(hash: BlockHash, requested: Requested): F[(BlockHash, Option[Requested])] =
      requested.waitingList match {
        case nextPeer :: waitingListTail =>
          def modifiedRequested(ts: Long) = requested.copy(
            timestamp = ts,
            waitingList = waitingListTail,
            peers = requested.peers + nextPeer
          )
          for {
            _ <- Log[F].debug(
                  s"Request ${PrettyPrinter.buildString(hash)} from ${nextPeer} " +
                    s"remain waiting ${waitingListTail.mkString(",")}"
                )
            _  <- requestForBlock(nextPeer, hash)
            ts <- Time[F].currentMillis
          } yield hash -> Option(modifiedRequested(ts))
        case _ =>
          val warnMessage = s"Could not retrieve requested block ${PrettyPrinter.buildString(hash)} " +
            s"from ${requested.peers.mkString(",")}. Removing the request from the requested blocks list. " +
            s"Casper will have to re-request the block."
          Metrics[F].incrementCounter("block-retrieve-failed") >>
            Log[F].warn(warnMessage).as(hash -> none[Requested])
      }

    import cats.instances.list._
    RequestedBlocks[F].flatModify(requests => {
      requests.keys.toList
        .traverse(hash => {
          val requested = requests(hash)
          Time[F].currentMillis
            .map(_ - requested.timestamp > timeout.seconds.toMillis)
            .ifM(tryRerequest(hash, requested), (hash -> Option(requested)).pure[F])
        })
        .map(list => toMap(list))
    })
  }

  def addNewEntry[F[_]: Monad: Log: Running.RequestedBlocks: Time](
      hash: BlockHash,
      peer: Option[PeerNode] = None
  ): F[Unit] =
    Log[F].info(s"Creating new entry for the ${PrettyPrinter.buildString(hash)} request") >> Time[F].currentMillis >>= (
        ts => RequestedBlocks.put(hash, Requested(timestamp = ts, peers = peer.toSet))
    )

  def handleHasBlock[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Time: Log](
      peer: PeerNode,
      hb: HasBlock
  )(
      casperContains: BlockHash => F[Boolean]
  ): F[Unit] = {

    val hashStr = PrettyPrinter.buildString(hb.hash)

    def addToWaitingList(requested: Requested): F[Unit] =
      Log[F].info(s"Adding $peer to waiting list of $hashStr request") >> RequestedBlocks.put(
        hb.hash,
        requested.copy(waitingList = requested.waitingList ++ List(peer))
      )

    val logIntro = s"Received confirmation from $peer that it has block $hashStr."
    Log[F].info(logIntro) >> casperContains(hb.hash).ifM(
      Applicative[F].unit,
      RequestedBlocks.get(hb.hash) >>= (_.fold(requestForNewBlock(peer, hb.hash))(
        req =>
          if (req.peers.isEmpty) requestForNewBlock(peer, hb.hash)
          else addToWaitingList(req)
      ))
    )
  }

  def handleHasBlockRequest[F[_]: Monad: RPConfAsk: TransportLayer](
      peer: PeerNode,
      hbr: HasBlockRequest
  )(blockLookup: BlockHash => F[Boolean]): F[Unit] =
    blockLookup(hbr.hash).flatMap(
      present =>
        Applicative[F].whenA(present)(
          sendToPeer(peer)(ToPacket(HasBlockProto(hbr.hash)))
        )
    )

  def handleBlockMessage[F[_]: Monad: Log: RequestedBlocks](peer: PeerNode, b: BlockMessage)(
      casperContains: BlockHash => F[Boolean],
      casperAdd: BlockMessage => F[ValidBlockProcessing]
  ): F[Unit] =
    casperContains(b.blockHash)
      .ifM(
        Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again."),
        Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.") >> casperAdd(b) >>= (
            status =>
              Applicative[F].whenA(BlockStatus.isInDag(status.merge))(
                RequestedBlocks.remove(b.blockHash)
              )
          )
      )

  def handleBlockHashMessage[F[_]: Monad: Log: ConnectionsCell: TransportLayer: Time: RPConfAsk: RequestedBlocks](
      peer: PeerNode,
      blockHashMessage: BlockHashMessage
  )(casperContains: BlockHash => F[Boolean]): F[Unit] =
    (
      casperContains(blockHashMessage.blockHash),
      RequestedBlocks.contains(blockHashMessage.blockHash)
    ).mapN(_ || _)
      .ifM(
        Log[F]
          .info(
            s"Received block hash ${PrettyPrinter.buildString(blockHashMessage.blockHash)} again."
          ),
        Log[F].info(
          s"Received block hash ${PrettyPrinter.buildString(blockHashMessage.blockHash)}. Requesting ..."
        ) *> requestForNewBlock(peer, blockHashMessage.blockHash)
      )

  def handleBlockRequest[F[_]: Monad: RPConfAsk: BlockStore: Log: TransportLayer](
      peer: PeerNode,
      br: BlockRequest
  ): F[Unit] =
    for {
      maybeBlock <- BlockStore[F].get(br.hash)
      logIntro   = s"Received request for block ${PrettyPrinter.buildString(br.hash)} from $peer."
      _ <- maybeBlock match {
            case None => Log[F].info(logIntro + "No response given since block not found.")
            case Some(block) =>
              streamToPeer(peer)(ToPacket(block.toProto)) <* Log[F].info(
                logIntro + "Response sent."
              )
          }
    } yield ()

  def handleForkChoiceTipRequest[F[_]: Sync: RPConfAsk: Log: TransportLayer: BlockStore](
      peer: PeerNode,
      fctr: ForkChoiceTipRequest.type
  )(casper: MultiParentCasper[F]): F[Unit] =
    Log[F].info(s"Received ForkChoiceTipRequest from $peer") >> MultiParentCasper.forkChoiceTip(
      casper
    ) >>= (
        tip =>
          streamToPeer(peer)(ToPacket(tip.toProto)) <* Log[F].info(
            s"Sending Block ${PrettyPrinter.buildString(tip.blockHash)} to $peer"
          )
      )

  def handleApprovedBlockRequest[F[_]: Monad: RPConfAsk: Log: TransportLayer](
      peer: PeerNode,
      br: ApprovedBlockRequest,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    Log[F].info(s"Received ApprovedBlockRequest from $peer") >> streamToPeer(peer)(
      ToPacket(approvedBlock.toProto)
    ) <* Log[F].info(s"Sending ApprovedBlock to $peer")

}

class Running[F[_]: Sync: BlockStore: CommUtil: TransportLayer: ConnectionsCell: RPConfAsk: Log: Time: Running.RequestedBlocks](
    casper: MultiParentCasper[F],
    approvedBlock: ApprovedBlock,
    validatorId: Option[ValidatorIdentity],
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  import Running._

  private val F    = Applicative[F]
  private val noop = F.unit

  private def casperAdd(peer: PeerNode)(b: BlockMessage): F[ValidBlockProcessing] = {
    import cats.instances.option._

    validatorId.traverse_ { id =>
      val self = ByteString.copyFrom(id.publicKey.bytes)
      F.whenA(b.sender == self)(
        Log[F].warn(
          s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
        )
      )
    } >>
      casper.addBlock(b)
  }

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case blockHash: BlockHashMessage => handleBlockHashMessage(peer, blockHash)(casper.contains)
    case b: BlockMessage             => handleBlockMessage(peer, b)(casper.contains, casperAdd(peer))
    case br: BlockRequest            => handleBlockRequest(peer, br)
    case hbr: HasBlockRequest        => handleHasBlockRequest(peer, hbr)(casper.contains)
    case hb: HasBlock                => handleHasBlock(peer, hb)(casper.contains)
    case _: ForkChoiceTipRequest.type =>
      handleForkChoiceTipRequest(peer, ForkChoiceTipRequest)(casper)
    case abr: ApprovedBlockRequest    => handleApprovedBlockRequest(peer, abr, approvedBlock)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable(na.nodeIdentifer)
    case _                            => noop
  }

  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = f(casper)
}
