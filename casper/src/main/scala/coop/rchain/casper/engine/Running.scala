package coop.rchain.casper.engine

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.ProtocolHelper.protocol
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Cell, Log, Time}
import coop.rchain.catscontrib.Catscontrib.ToBooleanF

import scala.concurrent.duration._

object Running {

  implicit private[this] val BlockRequesterMetricsSource =
    Metrics.Source(CasperMetricsSource, "block-requester")

  final case class Requested(
      timestamp: Long,
      peers: Set[PeerNode] = Set.empty,
      received: Boolean = false,
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
    * As we introduced synchrony constraint - there might be situation when node is stuck.
    * As an edge case with `sync = 0.99`, if node misses the block that is the last one to meet sync constraint,
    * it has no way to request it after it was broadcasted. So it will never meet synchrony constraint.
    * To mitigate this issue we can update fork choice tips if current fork-choice tip has old timestamp,
    * which means node does not propose new blocks and no new blocks were received recently.
    */
  def updateForkChoiceTipsIfStuck[F[_]: Sync: CommUtil: Log: Time: BlockStore: EngineCell](
      delayThreshold: FiniteDuration
  ): F[Unit] =
    for {
      engine <- EngineCell[F].read
      _ <- engine.withCasper(
            casper => {
              for {
                tip   <- MultiParentCasper.forkChoiceTip[F](casper)
                tipts = tip.header.timestamp
                _ <- Time[F].currentMillis
                      .map(_ - tipts > delayThreshold.toMillis)
                      .ifM(
                        Log[F].info(
                          "Updating fork choice tips as current FCT " +
                            s"is more then ${delayThreshold.toString} old. " +
                            s"Might be network is faulty."
                        ) >> CommUtil[F].sendForkChoiceTipRequest,
                        ().pure
                      )
              } yield ()
            },
            ().pure[F]
          )
    } yield ()

  /**
    * This method should be called periodically to maintain liveness of the protocol
    * and keep the requested blocks list clean.
    * See spec RunningMaintainRequestedBlocksSpec for more details
    */
  def maintainRequestedBlocks[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer: Log: Time: Metrics](
      timeout: FiniteDuration
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
          for {
            received <- requested.received.pure[F]
            expired  <- Time[F].currentMillis.map(_ - requested.timestamp > timeout.toMillis)
            rerequest <- if (expired && !received)
                          tryRerequest(hash, requested)
                        else
                          (hash -> Option(requested)).pure[F]
          } yield rerequest
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
      repeatedCasperMessage: BlockHash => F[Boolean]
  ): F[Unit] = {

    val hashStr = PrettyPrinter.buildString(hb.hash)

    def addToWaitingList(requested: Requested): F[Unit] =
      Log[F].info(s"Adding $peer to waiting list of $hashStr request") >> RequestedBlocks.put(
        hb.hash,
        requested.copy(waitingList = requested.waitingList ++ List(peer))
      )

    val logIntro = s"Received confirmation from $peer that it has block $hashStr."
    Log[F].info(logIntro) >> repeatedCasperMessage(hb.hash).ifM(
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

  def handleBlockMessage[F[_]: Monad: Log: RequestedBlocks](
      peer: PeerNode,
      b: BlockMessage
  )(
      repeatedCasperMessage: BlockHash => F[Boolean],
      isInBlockStore: BlockHash => F[Boolean],
      casperAdd: BlockMessage => F[ValidBlockProcessing]
  ): F[Unit] = {
    val alreadyInStore = isInBlockStore(b.blockHash)
    (repeatedCasperMessage(b.blockHash) ||^ alreadyInStore)
      .ifM(
        Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again.") >>
          alreadyInStore.ifM(RequestedBlocks.remove(b.blockHash), ().pure[F]),
        Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.") >> casperAdd(b) >>= (
            status =>
              Applicative[F].whenA(BlockStatus.isInDag(status.merge))(
                RequestedBlocks.remove(b.blockHash)
              )
          )
      )
  }

  def handleBlockHashMessage[F[_]: Monad: Log: ConnectionsCell: TransportLayer: Time: RPConfAsk: RequestedBlocks](
      peer: PeerNode,
      blockHashMessage: BlockHashMessage
  )(repeatedCasperMessage: BlockHash => F[Boolean]): F[Unit] =
    repeatedCasperMessage(blockHashMessage.blockHash)
      .ifM(
        Log[F]
          .info(
            s"Received block hash ${PrettyPrinter.buildString(blockHashMessage.blockHash)} again."
          ),
        Log[F].info(
          s"Received block hash ${PrettyPrinter.buildString(blockHashMessage.blockHash)}. Requesting ..."
        ) >> requestForNewBlock(peer, blockHashMessage.blockHash)
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
              streamToPeer(peer)(ToPacket(block.toProto)) >> Log[F].info(
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
          Log[F].info(
            s"Streaming block ${PrettyPrinter.buildString(tip.blockHash)} to $peer"
          ) >> streamToPeer(peer)(ToPacket(tip.toProto))
      )

  def handleApprovedBlockRequest[F[_]: Monad: RPConfAsk: Log: TransportLayer](
      peer: PeerNode,
      br: ApprovedBlockRequest,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    Log[F].info(s"Received ApprovedBlockRequest from $peer") >> streamToPeer(peer)(
      ToPacket(approvedBlock.toProto)
    ) >> Log[F].info(s"ApprovedBlock sent to $peer")

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

  /**
    * Message that relates to a block (e.g. block message or block hash broadcast message) shall be considered
    * as repeated and dropped if corresponding block is marked as received in RequestedBlocks or already added to DAG.
    * Otherwise - that is a new message and shall be processed normal way.
    *
    * RequestedBlocks is a part of Casper loop maintaining liveness of the protocol.
    * It keeping records of blocks being requested and re-request them on schedule if failed first time.
    * It is expected to get lots of duplicate messages from different peers, so check for duplicate is required.
    * Processing of duplicate blocks puts load on Casper and highly discouraged.
    *
    * When block is received from some peer first time - it is
    * - marked as `received` in RequestedBlocks,
    * - added to the block store and
    * - sent to Casper.
    * Block is being kept in RequestedBlocks until it is added to the DAG so we can detect duplicates.
    *
    * If either RequestedBlocks shows that block was received (but not yet added to Casper)
    * or block was already added by Casper (so removed from RequestedBlocks) - related message
    * considered as repeated.
    *
    * @param hash Block hash
    * @return If message is repeated
    */
  private def repeatedCasperMessage(hash: BlockHash): F[Boolean] =
    RequestedBlocks.get(hash).map(x => x.nonEmpty && x.get.received) ||^ casper.contains(hash)

  /**
    * Wrapper for Casper add block.
    * @param peer originator of the block
    * @param b block
    * @return block processing result
    */
  private def casperAdd(peer: PeerNode)(b: BlockMessage): F[ValidBlockProcessing] = {
    import cats.instances.option._

    // Save block in block store immediately
    BlockStore[F].put(b) >>
      validatorId.traverse_ { id =>
        val self = ByteString.copyFrom(id.publicKey.bytes)
        F.whenA(b.sender == self)(
          Log[F].warn(
            s"There is another node $peer proposing using the same private key as you. Or did you restart your node?"
          )
        )
      } >> {
      for {
        maybeReq <- RequestedBlocks.get(b.blockHash)
        _ <- maybeReq match {
              // There might be blocks that are not maintained by RequestedBlocks, e.g. genesis ceremony messages
              case None =>
                Log[F].info(
                  s"Block ${PrettyPrinter.buildString(b.blockHash)} is not present in RequestedBlocks."
                )
              case Some(requested) => {
                // Make Casper loop aware that the block has been received
                RequestedBlocks.put(b.blockHash, requested.copy(received = true)) >>
                  Log[F].info(
                    s"Block ${PrettyPrinter.buildString(b.blockHash)} marked as received."
                  )
              }
            }
      } yield ()
    } >> processUnfinishedParentsAndAddBlock(b.blockHash)
  }

  /**
    * Add parents that are already in block store and then block itself
    * @param b block
    * @return Result of block processing
    */
  private def processUnfinishedParentsAndAddBlock(b: BlockHash): F[ValidBlockProcessing] =
    processUnfinishedParents(b) >> BlockStore[F].get(b).map(_.get) >>= casper.addBlock

  val blockCounter = Ref.unsafe(0)

  /**
    * Adds parents that are in block store but not in DAG yet
    * @param b block
    * @return F[Unit]
    */
  private def processUnfinishedParents(hash: BlockHash): F[Unit] = {
    import cats.instances.list._
    for {
      counter <- blockCounter.modify(c => (c + 1, c + 1))
      // Parents that are in block store but not in DAG yet and not being currently handled
      b <- BlockStore[F].get(hash).map(_.get)
      _ <- Log[F].info(
            s"Unfinished blocks counter ${counter} #${b.body.state.blockNumber} ${PrettyPrinter.buildString(hash)}"
          )
      unfinishedParents <- b.header.parentsHashList.traverse { hash =>
                            for {
                              block <- repeatedCasperMessage(hash)
                                        .ifM(none[BlockMessage].pure[F], BlockStore[F].get(hash))
                            } yield block.map(_.blockHash)
                          }
      // Try to add block to DAG. We don't care of results, just make an attempt.
      _ <- unfinishedParents.flatten.traverse_(processUnfinishedParentsAndAddBlock)
      _ <- blockCounter.update(_ - 1)
    } yield ()
  }

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case blockHash: BlockHashMessage =>
      handleBlockHashMessage(peer, blockHash)(repeatedCasperMessage)
    case b: BlockMessage =>
      handleBlockMessage(peer, b)(repeatedCasperMessage, BlockStore[F].contains, casperAdd(peer))
    case br: BlockRequest     => handleBlockRequest(peer, br)
    case hbr: HasBlockRequest => handleHasBlockRequest(peer, hbr)(casper.contains)
    case hb: HasBlock         => handleHasBlock(peer, hb)(repeatedCasperMessage)
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
