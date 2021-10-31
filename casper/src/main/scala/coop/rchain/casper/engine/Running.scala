package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.casper._
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.PeerNode
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceStateManager}
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Time}
import fs2.Stream

import scala.concurrent.duration._

object Running {

  implicit val MetricsSource: Metrics.Source =
    Metrics.Source(CasperMetricsSource, "running")

  trait CasperMessageStatus
  final case object BlockIsInDag            extends CasperMessageStatus
  final case object BlockIsInCasperBuffer   extends CasperMessageStatus
  final case object BlockIsReceived         extends CasperMessageStatus
  final case object BlockIsWaitingForCasper extends CasperMessageStatus
  final case object BlockIsInProcessing     extends CasperMessageStatus
  final case object DoNotIgnore             extends CasperMessageStatus

  final case class IgnoreCasperMessageStatus(doIgnore: Boolean, status: CasperMessageStatus)

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
                latestMessages <- casper.blockDag.flatMap(
                                   _.latestMessageHashes.map(_.values.toSet)
                                 )
                now <- Time[F].currentMillis
                hasRecentLatestMessage = Stream
                  .fromIterator(latestMessages.iterator)
                  .evalMap(BlockStore[F].getUnsafe)
                  // filter only blocks that are recent
                  .filter { b =>
                    val blockTimestamp = b.header.timestamp
                    (now - blockTimestamp) < delayThreshold.toMillis
                  }
                  .head
                  .compile
                  .last
                  .map(_.isDefined)
                stuck <- hasRecentLatestMessage.not
                requestWithLog = Log[F].info(
                  "Requesting tips update as newest latest message " +
                    s"is more then ${delayThreshold.toString} old. " +
                    s"Might be network is faulty."
                ) >> CommUtil[F].sendForkChoiceTipRequest

                _ <- (requestWithLog).whenA(stuck)
              } yield ()
            },
            ().pure[F]
          )
    } yield ()

  /**
    * Peer broadcasted block hash.
    */
  def handleBlockHashMessage[F[_]: Monad: BlockRetriever: Log](
      peer: PeerNode,
      bhm: BlockHashMessage
  )(
      ignoreMessageF: BlockHash => F[Boolean]
  ): F[Unit] = {
    val h = bhm.blockHash
    def logIgnore = Log[F].debug(
      s"Ignoring ${PrettyPrinter.buildString(h)} hash broadcast"
    )
    val logSuccess = Log[F].debug(
      s"Incoming BlockHashMessage ${PrettyPrinter.buildString(h)} " +
        s"from ${peer.endpoint.host}"
    )
    val processHash =
      BlockRetriever[F].admitHash(h, peer.some, BlockRetriever.HashBroadcastRecieved)

    ignoreMessageF(h).ifM(
      logIgnore,
      logSuccess >> processHash.void
    )
  }

  /**
    * Peer says it has particular block.
    */
  def handleHasBlockMessage[F[_]: Monad: BlockRetriever: Log](
      peer: PeerNode,
      hb: HasBlock
  ): F[Unit] =
    BlockRetriever[F].admitHash(hb.hash, peer.some, BlockRetriever.HasBlockMessageReceived).void

  /**
    * Peer asks for particular block
    */
  def handleBlockRequest[F[_]: Monad: TransportLayer: RPConfAsk: BlockStore: Log](
      peer: PeerNode,
      br: BlockRequest
  ): F[Unit] = {
    val getBlock = BlockStore[F].get(br.hash).flatMap(_.get.pure[F])
    val logSuccess = Log[F].info(
      s"Received request for block ${PrettyPrinter.buildString(br.hash)} " +
        s"from $peer. Response sent."
    )
    val logError = Log[F].info(
      s"Received request for block ${PrettyPrinter.buildString(br.hash)} " +
        s"from $peer. No response given since block not found."
    )
    def sendResponse(block: BlockMessage) =
      TransportLayer[F].streamToPeer(peer, block.toProto)
    val hasBlock = BlockStore[F].contains(br.hash)

    hasBlock.ifM(
      logSuccess >> getBlock >>= sendResponse,
      logError
    )
  }

  /**
    * Peer asks if this node has particular block
    */
  def handleHasBlockRequest[F[_]: Monad: TransportLayer: RPConfAsk](
      peer: PeerNode,
      hbr: HasBlockRequest
  )(blockLookup: BlockHash => F[Boolean]): F[Unit] = {
    val hasBlock  = blockLookup(hbr.hash)
    val sendBlock = TransportLayer[F].sendToPeer(peer, HasBlockProto(hbr.hash))

    hasBlock.ifM(sendBlock, ().pure[F])
  }

  /**
    * Peer asks for fork-choice tip
    */
  // TODO name for this message is misleading, as its a request for all tips, not just fork choice.
  def handleForkChoiceTipRequest[F[_]: Sync: TransportLayer: RPConfAsk: BlockStore: Log](
      peer: PeerNode
  )(getTips: F[Iterable[BlockHash]]): F[Unit] = {
    val logRequest = Log[F].info(s"Received ForkChoiceTipRequest from ${peer.endpoint.host}")
    def logResponse(tips: Iterable[BlockHash]): F[Unit] =
      Log[F].info(
        s"Sending tips ${PrettyPrinter.buildString(tips)} to ${peer.endpoint.host}"
      )

    // TODO respond with all tips in a single message
    def respondToPeer(tip: BlockHash) = TransportLayer[F].sendToPeer(peer, HasBlockProto(tip))

    logRequest >> getTips.flatMap(t => t.toList.traverse(respondToPeer) >> logResponse(t))
  }

  /**
    * Peer asks for ApprovedBlock
    */
  def handleApprovedBlockRequest[F[_]: Monad: TransportLayer: RPConfAsk: Log](
      peer: PeerNode,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    Log[F].info(s"Received ApprovedBlockRequest from ${peer}") >>
      TransportLayer[F].streamToPeer(peer, approvedBlock.toProto) >>
      Log[F].info(s"ApprovedBlock sent to ${peer}")

  final case object LastFinalizedBlockNotFoundError
      extends Exception("Last finalized block not found in the block storage.")

  private def handleStateItemsMessageRequest[F[_]: Sync: TransportLayer: RPConfAsk: RSpaceStateManager: Log](
      peer: PeerNode,
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int
  ): F[Unit] = {
    import coop.rchain.rspace.syntax._
    for {
      // Export chunk of store items from RSpace
      exportedItems <- RSpaceStateManager[F].exporter.getHistoryAndData(
                        startPath,
                        skip,
                        take,
                        ByteString.copyFrom
                      )

      (history, data) = exportedItems

      // Prepare response with the chunk of store items
      resp = StoreItemsMessage(startPath, history.lastPath, history.items, data.items)

      _ <- Log[F].info(s"Read ${resp.pretty}")

      // Stream store items to peer
      respProto = StoreItemsMessage.toProto(resp)
      _         <- TransportLayer[F].streamToPeer(peer, respProto)

      _ <- Log[F].info(s"Store items sent to $peer")
    } yield ()
  }
}

// format: off
class Running[F[_]
  /* Execution */   : Concurrent: Time
  /* Transport */   : TransportLayer: CommUtil: BlockRetriever
  /* State */       : RPConfAsk: ConnectionsCell
  /* Storage */     : BlockStore: BlockDagStorage: RSpaceStateManager
  /* Diagnostics */ : Log: Metrics] // format: on
(
    casper: MultiParentCasper[F],
    blockDagStateRef: Ref[F, BlockDagState],
    approvedBlock: ApprovedBlock,
    validatorId: Option[ValidatorIdentity],
    theInit: F[Unit],
    disableStateExporter: Boolean,
    processBlockInRunning: BlockMessage => F[Unit]
) extends Engine[F] {

  import Engine._
  import Running._

  private val F    = Applicative[F]
  private val noop = F.unit

  private def ignoreCasperMessage(hash: BlockHash): F[Boolean] =
    blockDagStateRef.get.map(_.received(hash))

  override def init: F[Unit] = theInit

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case b: BlockMessage => processBlockInRunning(b)
    case h: BlockHashMessage =>
      handleBlockHashMessage(peer, h)(
        ignoreCasperMessage
      )

    case br: BlockRequest => handleBlockRequest(peer, br)
    // TODO should node say it has block only after it is in DAG, or CasperBuffer is enough? Or even just BlockStore?
    // https://github.com/rchain/rchain/pull/2943#discussion_r449887701
    case hbr: HasBlockRequest =>
      handleHasBlockRequest(peer, hbr) { h =>
        blockDagStateRef.get.map(_.validated.dagSet.contains(h))
      }
    case hb: HasBlock => ignoreCasperMessage(hb.hash).ifM(().pure, handleHasBlockMessage(peer, hb))
    case _: ForkChoiceTipRequest.type =>
      handleForkChoiceTipRequest(peer)(
        blockDagStateRef.get.map(_.validated.latestMessagesMap.values)
      )
    case abr: ApprovedBlockRequest =>
      for {
        lfBlockHash <- BlockDagStorage[F].getRepresentation
                        .flatMap(_.genesis) //.flatMap(_.lastFinalizedBlock)

        // Create approved block from last finalized block
        lastFinalizedBlock = for {
          lfBlock <- BlockStore[F].getUnsafe(lfBlockHash)

          // Each approved block should be justified by validators signatures
          // ATM we have signatures only for genesis approved block - we also have to have a procedure
          // for gathering signatures for each approved block post genesis.
          // Now new node have to trust bootstrap if it wants to trim state when connecting to the network.
          // TODO We need signatures of Validators supporting this block
          lastApprovedBlock = ApprovedBlock(
            ApprovedBlockCandidate(lfBlock, 0),
            List.empty
          )
        } yield lastApprovedBlock

        approvedBlock <- if (abr.trimState)
                          // If Last Finalized State is requested return Last Finalized block as Approved block
                          lastFinalizedBlock
                        else
                          // Respond with approved block that this node is started from.
                          // The very first one is genesis, but this node still might start from later block,
                          // so it will not necessary be genesis.
                          approvedBlock.pure[F]

        _ <- handleApprovedBlockRequest(peer, approvedBlock)
      } yield ()
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable(na.nodeIdentifer)

    // Approved state store records
    case StoreItemsMessageRequest(startPath, skip, take) =>
      val start = startPath.map(RSpaceExporter.pathPretty).mkString(" ")
      val logRequest = Log[F].info(
        s"Received request for store items, startPath: [$start], chunk: $take, skip: $skip, from: $peer"
      )
      if (!disableStateExporter) {
        logRequest *> handleStateItemsMessageRequest(peer, startPath, skip, take)
      } else {
        Log[F].info(
          s"Received StoreItemsMessage request but the node is configured to not respond to StoreItemsMessage, from ${peer}."
        )
      }
    case _ => noop
  }

  override def withCasper[A](
      f: MultiParentCasper[F] => F[A],
      default: F[A]
  ): F[A] =
    f(casper)
}
