package coop.rchain.casper.engine

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.{Applicative, Monad}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.{mkPacket, protocol}
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{Cell, Log, Time}

import scala.concurrent.duration._

object Running {

  val timeout: FiniteDuration = 240 seconds

  final case class Requested(
      timestamp: Long,
      peers: Set[PeerNode] = Set.empty,
      waitingList: List[PeerNode] = List.empty
  )

  type RequestedBlocks[F[_]] = Cell[F, Map[BlockHash, Requested]]
  object RequestedBlocks {
    def apply[F[_]: RequestedBlocks]: RequestedBlocks[F] = implicitly[RequestedBlocks[F]]
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
  ) = sendToPeer(peer)(mkPacket(transport.BlockRequest, BlockRequest(hash)))

  private def requestForNewBlock[F[_]: Monad: RPConfAsk: TransportLayer: RequestedBlocks: Log: Time](
      peer: PeerNode,
      blockHash: BlockHash
  ) = requestForBlock(peer, blockHash) >> addNewEntry(blockHash, Some(peer))

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
          _  <- requestForBlock(nextPeer, hash)
          ts <- Time[F].currentMillis
        } yield ((hash -> Option(modifiedRequested(ts))))
      } else {
        val warnMessage = s"Could not retrieve requested block ${PrettyPrinter.buildString(hash)}. " +
          "Removing the request from the requested blocks list. Casper will have to re-request the block."
        Log[F].warn(warnMessage).as((hash -> none[Requested]))
      }

    import cats.instances.list._
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

  def addNewEntry[F[_]: Monad: Log: Running.RequestedBlocks: Time](
      hash: BlockHash,
      peer: Option[PeerNode] = None
  ): F[Unit] =
    Log[F].info(s"Creating new entry for the ${PrettyPrinter.buildString(hash)} request") >> Time[F].currentMillis >>= (
        ts => {
          RequestedBlocks[F].modify(
            _ + (hash -> Requested(timestamp = ts, peers = peer.toSet))
          )
        }
    )

  def handleHasBlock[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Time: Log](
      peer: PeerNode,
      hb: HasBlock
  )(
      casperContains: BlockHash => F[Boolean]
  ): F[Unit] = {

    val hashStr = PrettyPrinter.buildString(hb.hash)

    def addToWaitingList(requested: Requested): F[Unit] =
      Log[F].info(s"Adding $peer to waiting list of $hashStr request") >> RequestedBlocks[F]
        .modify { requestedBlocks =>
          requestedBlocks + (hb.hash -> requested.copy(
            waitingList = requested.waitingList ++ List(peer)
          ))
        }

    val logIntro = s"Received confirmation from $peer that it has block $hashStr."
    Log[F].info(logIntro) >> casperContains(hb.hash).ifM(
      Applicative[F].unit,
      RequestedBlocks[F].read >>= (_.get(hb.hash).fold(requestForNewBlock(peer, hb.hash))(
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
          sendToPeer(peer)(mkPacket(transport.HasBlock, HasBlock(hbr.hash)))
        )
    )

  def handleBlockMessage[F[_]: Monad: Log: RequestedBlocks](peer: PeerNode, b: BlockMessage)(
      casperContains: BlockHash => F[Boolean],
      casperAdd: BlockMessage => F[ValidBlockProcessing]
  ): F[Unit] =
    casperContains(b.blockHash)
      .ifM(
        Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again."),
        Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.") *> casperAdd(b) >>= (
            status =>
              Applicative[F].whenA(BlockStatus.isInDag(status.merge))(
                RequestedBlocks[F].modify(_ - b.blockHash)
              )
          )
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
              streamToPeer(peer)(mkPacket(transport.BlockMessage, block)) <* Log[F].info(
                logIntro + "Response sent."
              )
          }
    } yield ()

  def handleForkChoiceTipRequest[F[_]: Sync: RPConfAsk: Log: TransportLayer: BlockStore](
      peer: PeerNode,
      fctr: ForkChoiceTipRequest.type
  )(casper: MultiParentCasper[F]): F[Unit] =
    Log[F].info(s"Received ForkChoiceTipRequest from $peer") *> MultiParentCasper.forkChoiceTip(
      casper
    ) >>= (
        tip =>
          streamToPeer(peer)(mkPacket(transport.BlockMessage, tip)) <* Log[F].info(
            s"Sending Block ${tip.blockHash} to $peer"
          )
      )

  def handleApprovedBlockRequest[F[_]: Monad: RPConfAsk: Log: TransportLayer](
      peer: PeerNode,
      br: ApprovedBlockRequest,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    Log[F].info(s"Received ApprovedBlockRequest from $peer") *> streamToPeer(peer)(
      mkPacket(transport.ApprovedBlock, approvedBlock)
    ) <* Log[F].info(s"Sending ApprovedBlock to $peer")

}

class Running[F[_]: Sync: RPConfAsk: BlockStore: ConnectionsCell: TransportLayer: Log: Time: Running.RequestedBlocks](
    casper: MultiParentCasper[F],
    approvedBlock: ApprovedBlock,
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._
  import Running._

  private val F    = Applicative[F]
  private val noop = F.unit

  private def casperAdd(peer: PeerNode)(b: BlockMessage): F[ValidBlockProcessing] = {
    val handleDoppelganger: (BlockMessage, Validator) => F[Unit] =
      (bm: BlockMessage, self: Validator) =>
        F.whenA(bm.sender == self)(
          Log[F].warn(
            s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
          )
        )
    casper.addBlock(b, handleDoppelganger)
  }

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case b: BlockMessage              => handleBlockMessage(peer, b)(casper.contains, casperAdd(peer))
    case br: BlockRequest             => handleBlockRequest(peer, br)
    case hbr: HasBlockRequest         => handleHasBlockRequest(peer, hbr)(casper.contains)
    case hb: HasBlock                 => handleHasBlock(peer, hb)(casper.contains)
    case fctr: ForkChoiceTipRequest   => handleForkChoiceTipRequest(peer, fctr)(casper)
    case abr: ApprovedBlockRequest    => handleApprovedBlockRequest(peer, abr, approvedBlock)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable(na.nodeIdentifer)
    case _                            => noop
  }

  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] = f(casper)
}
