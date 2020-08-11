package coop.rchain.casper.engine

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferStorage
import coop.rchain.blockstorage.finality.LastFinalizedStorage
import coop.rchain.casper._
import coop.rchain.casper.syntax._
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.PeerNode
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Log, Time}
import coop.rchain.catscontrib.Catscontrib.ToBooleanF
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.state.RSpaceStateManager

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._

object Running {

  implicit val MetricsSource: Metrics.Source =
    Metrics.Source(CasperMetricsSource, "running")

  trait CasperMessageStatus
  final case object BlockIsInDag            extends CasperMessageStatus
  final case object BlockIsInCasperBuffer   extends CasperMessageStatus
  final case object BlockIsWaitingForCasper extends CasperMessageStatus
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
                tipHash      <- MultiParentCasper.forkChoiceTip[F](casper)
                tip          <- BlockStore[F].getUnsafe(tipHash)
                tipTimestamp = tip.header.timestamp
                now          <- Time[F].currentMillis
                expired      = (now - tipTimestamp) > delayThreshold.toMillis
                requestWithLog = Log[F].info(
                  "Updating fork choice tips as current FCT " +
                    s"is more then ${delayThreshold.toString} old. " +
                    s"Might be network is faulty."
                ) >> CommUtil[F].sendForkChoiceTipRequest

                _ <- (requestWithLog).whenA(expired)
              } yield ()
            },
            ().pure[F]
          )
    } yield ()

  /**
    * Peer sent the block
    *
    * @param blockMessage BlockMessage
    * @param peer Sender of the message
    * @param ignoreMessageF Defines when to ignore message.
    * @param validateBlockF Defines basic block validation before persisting.
    * @param casperAddF Function that sends block to Casper for inserting in the DAG
    * @param lockMap       BlockMessage is a heavy message, so excessive processing
    *                      of duplicate BlockMessages should be discouraged. This is map of Semaphore to block hash.
    * @return
    */
  def handleBlockMessage[F[_]: Concurrent: TransportLayer: BlockRetriever: BlockStore: Metrics: Log](
      blockMessage: BlockMessage,
      peer: PeerNode
  )(
      ignoreMessageF: BlockHash => F[IgnoreCasperMessageStatus],
      validateBlockF: (BlockMessage, PeerNode) => F[Boolean],
      casperAddF: BlockHash => F[Unit],
      lockMap: TrieMap[BlockHash, Semaphore[F]]
  ): F[Unit] = {
    val logIncoming = Log[F].info(
      s"Incoming BlockMessage ${PrettyPrinter.buildString(blockMessage, short = true)} " +
        s"from ${peer.endpoint.host}. Number of hashes currently handled is ${lockMap.size}."
    )
    val blockStr = PrettyPrinter.buildString(blockMessage, short = true)
    def logIgnore(s: CasperMessageStatus) = Log[F].info(
      s"Ignoring BlockMessage ${blockStr} because of ${s}."
    )
    val logAlreadyInStore = Log[F].info(
      s"Block ${blockStr} is available in BlockStore. Sending to Casper."
    )
    val logOkSaved = Log[F].info(
      s"Block ${blockStr} is good. Saved to BlockStore. Sending to Casper."
    )
    val logBadDropped = Log[F].warn(
      s"Ignoring BlockMessage ${blockStr} because its malformed or not of our interest."
    )
    val hasBlock   = BlockStore[F].contains(blockMessage.blockHash)
    val ackReceive = BlockRetriever[F].ackReceive(blockMessage.blockHash)
    val saveBlock  = BlockStore[F].put(blockMessage)

    for {
      _         <- logIncoming
      semaphore <- MetricsSemaphore.single
      hashLock  = lockMap.getOrElseUpdate(blockMessage.blockHash, semaphore)
      proceed <- hashLock.withPermit {
                  ignoreMessageF(blockMessage.blockHash)
                    .flatMap(r => {
                      if (r.doIgnore) {
                        logIgnore(r.status) >> false.pure[F]
                      } else {
                        hasBlock.ifM(
                          logAlreadyInStore >> ackReceive >> true.pure[F],
                          validateBlockF(blockMessage, peer).ifM(
                            saveBlock >> logOkSaved >> ackReceive >> true
                              .pure[F],
                            logBadDropped >> false.pure[F]
                          )
                        )
                      }
                    })
                }
      _ <- lockMap.remove(blockMessage.blockHash).pure[F]
      _ <- if (proceed) casperAddF(blockMessage.blockHash) else ().pure[F]
    } yield ()
  }

  /**
    * Peer broadcasted block hash.
    */
  def handleBlockHashMessage[F[_]: Monad: BlockRetriever: Log](
      peer: PeerNode,
      bhm: BlockHashMessage
  )(
      ignoreMessageF: BlockHash => F[IgnoreCasperMessageStatus]
  ): F[Unit] = {
    val h = bhm.blockHash
    def logIgnore(s: CasperMessageStatus) = Log[F].debug(
      s"Ignoring ${PrettyPrinter.buildString(h)} hash broadcast with status: ${s}"
    )
    val logSuccess = Log[F].debug(
      s"Incoming BlockHashMessage ${PrettyPrinter.buildString(h)} " +
        s"from ${peer.endpoint.host}"
    )
    val processHash =
      BlockRetriever[F].admitHash(h, peer.some, BlockRetriever.HashBroadcastRecieved)

    ignoreMessageF(h).flatMap(r => {
      if (r.doIgnore) {
        logIgnore(r.status)
      } else {
        logSuccess >> processHash.void
      }
    })
  }

  /**
    * Peer says it has particular block.
    */
  def handleHasBlockMessage[F[_]: Monad: BlockRetriever: Log](
      peer: PeerNode,
      hb: HasBlock
  )(
      ignoreMessageF: BlockHash => F[IgnoreCasperMessageStatus]
  ): F[Unit] = {
    val h = hb.hash
    def logIgnore(s: CasperMessageStatus) = Log[F].debug(
      s"Ignoring ${PrettyPrinter.buildString(h)} HasBlockMessage with status: ${s}"
    )
    val logProcess = Log[F].debug(
      s"Incoming HasBlockMessage ${PrettyPrinter.buildString(h)} from ${peer.endpoint.host}"
    )
    val processHash =
      BlockRetriever[F].admitHash(h, peer.some, BlockRetriever.HasBlockMessageReceived)

    ignoreMessageF(h).flatMap(r => {
      if (r.doIgnore) {
        logIgnore(r.status)
      } else {
        logProcess >> processHash.void
      }
    })
  }

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
  def handleForkChoiceTipRequest[F[_]: Sync: TransportLayer: RPConfAsk: BlockStore: Log](
      peer: PeerNode
  )(casper: MultiParentCasper[F]): F[Unit] = {
    val logRequest = Log[F].info(s"Received ForkChoiceTipRequest from ${peer.endpoint.host}")
    def logResponse(blockHash: BlockHash) =
      Log[F].info(
        s"Sending hash ${PrettyPrinter.buildString(blockHash)} to ${peer.endpoint.host}"
      )
    val getTip = MultiParentCasper.forkChoiceTip(casper)
    def respondToPeer(tip: BlockHash) =
      logResponse(tip) >> TransportLayer[F].sendToPeer(peer, HasBlockProto(tip))

    logRequest >> getTip >>= respondToPeer
  }

  /**
    * Peer asks for ApprovedBlock
    */
  def handleApprovedBlockRequest[F[_]: Monad: TransportLayer: RPConfAsk: Log](
      peer: PeerNode,
      approvedBlock: ApprovedBlock
  ): F[Unit] =
    Log[F].info(s"Received ApprovedBlockRequest from ${peer.endpoint.host}") >>
      TransportLayer[F].streamToPeer(peer, approvedBlock.toProto) >>
      Log[F].info(s"ApprovedBlock sent to ${peer.endpoint.host}")

  final case object LastFinalizedBlockNotFoundError
      extends Exception("Last finalized block not found in the block storage.")

  private def handleStateItemsMessageRequest[F[_]: Sync: TransportLayer: RPConfAsk: RSpaceStateManager: Log](
      peer: PeerNode,
      startPath: Seq[(Blake2b256Hash, Option[Byte])],
      skip: Int,
      take: Int
  ): F[Unit] = {
    import coop.rchain.rspace.state.syntax._
    for {
      history <- RSpaceStateManager[F].exporter.getHistory(
                  startPath,
                  skip,
                  take,
                  ByteString.copyFrom
                )
      data <- RSpaceStateManager[F].exporter.getData(
               startPath,
               skip,
               take,
               ByteString.copyFrom
             )
      _ = println(
        s"HISTORY READ history: ${history.items.size}, data: ${data.items.size}, path: $startPath"
      )
      req      = StoreItemsMessage(startPath, history.lastPath, history.items, data.items)
      reqProto = StoreItemsMessage.toProto(req)
      _        <- TransportLayer[F].streamToPeer(peer, reqProto)
      _        <- Log[F].info(s"Store items sent to $peer")
    } yield ()
  }

  // format: off
  class Running[F[_]
    /* Execution */   : Concurrent: Time
    /* Transport */   : TransportLayer: CommUtil: BlockRetriever
    /* State */       : RPConfAsk: ConnectionsCell
    /* Storage */     : BlockStore: LastFinalizedStorage: CasperBufferStorage: RSpaceStateManager
    /* Diagnostics */ : Log: Metrics] // format: on
  (
      casper: MultiParentCasper[F],
      approvedBlock: ApprovedBlock,
      validatorId: Option[ValidatorIdentity],
      theInit: F[Unit]
  ) extends Engine[F] {

    import Engine._

    private val F    = Applicative[F]
    private val noop = F.unit

    /**
      * Message that relates to a block A (e.g. block message or block hash broadcast message) shall be considered
      * as repeated and ignored if block A is
      * 1. marked as received in BlocksRetriever and pending adding to CasperBuffer or
      * 2. added do CasperBuffer and pending adding to DAG or
      * 3. already added to DAG.
      * Otherwise - that is a new message and shall be processed normal way.
      *
      * @param hash Block hash
      * @return If message should be ignored
      */
    private def ignoreCasperMessage(hash: BlockHash): F[IgnoreCasperMessageStatus] =
      BlockRetriever[F]
        .received(hash)
        .ifM(
          IgnoreCasperMessageStatus(doIgnore = true, BlockIsWaitingForCasper).pure[F],
          CasperBufferStorage[F]
            .contains(hash)
            .ifM(
              IgnoreCasperMessageStatus(doIgnore = true, BlockIsInCasperBuffer).pure[F],
              casper.blockDag.flatMap(
                _.contains(hash)
                  .ifM(
                    IgnoreCasperMessageStatus(doIgnore = true, BlockIsInDag).pure[F],
                    IgnoreCasperMessageStatus(doIgnore = false, DoNotIgnore).pure[F]
                  )
              )
            )
        )

    /**
      * Basic block validation before saving block in Blockstore. The intention here is to prevent saving
      * bad and useless blocks. So the check is if block is well formed and of interest for this particular node.
      * Should not catch any slashable offences - such blocks should be persisted and processed by Casper.
      *
      * @param b    BlockMessage
      * @param peer peer from which BlockMessage came from
      * @return if block is suitable for this node
      */
    private def blockIsOk(
        b: BlockMessage,
        peer: PeerNode
    ): F[Boolean] =
      withCasper(
        casper =>
          for {
            _ <- validatorId match {
                  case None => ().pure[F]
                  case Some(id) =>
                    F.whenA(b.sender == ByteString.copyFrom(id.publicKey.bytes))(
                      Log[F].warn(
                        s"There is another node $peer proposing using the same private key as you. " +
                          s"Or did you restart your node?"
                      )
                    )
                }
            ab = approvedBlock.candidate.block
            // TODO might be more checks can/should be applied here
            validShard   = ab.shardId.equalsIgnoreCase(b.shardId)
            validFormat  <- Validate.formatOfFields(b)
            validSig     <- Validate.blockSignature(b)
            validVersion <- casper.getVersion.flatMap(Validate.version(b, _))
            oldBlock     = ProtoUtil.blockNumber(b) < ProtoUtil.blockNumber(ab)
            _ <- Log[F]
                  .warn(
                    s"Block is bad: from wrong shard: ${b.shardId}, this node participates in: [${ab.shardId}]."
                  )
                  .unlessA(validShard)
            _ <- Log[F].warn(s"Block is bad: ill formed.").unlessA(validFormat)
            _ <- Log[F].warn(s"Block is bad: wrong signature.").unlessA(validSig)
            _ <- Log[F].warn(s"Block is bad: wrong version.").unlessA(validVersion)
            _ <- Log[F]
                  .warn(s"Block is old: we don't need siblings or parents of approvedBlock")
                  .whenA(oldBlock)

            isValid = validShard && validFormat && validSig && validVersion && !oldBlock
          } yield isValid,
        Log[F].info("Casper is not ready, rejecting all blocks until it is ready.") >> false.pure[F]
      )

    /**
      * Sends block to from BlockStore to Casper for inserting to the DAG.
      *
      * @param bh block hash
      * @return Result of block processing
      */
    private def sendBlockFromStoreToCasper(bh: BlockHash): F[Unit] =
      casper.addBlockFromStore(bh) >>= (
          // At this stage block should be in CasperBuffer, or even in the DAG.
          // It might wait for dependencies so not added to DAG, but there is no point to rerequest it again
          status =>
            BlockRetriever[F].ackInCasper(bh) >> Log[F].info(
              s"Block ${PrettyPrinter.buildString(bh)} adding finished with status: $status. " +
                s"BlockRetriever is informed that block consumed by Casper."
            )
        )

    override def init: F[Unit] = theInit

    private[this] val hashHandlerLock = TrieMap.empty[BlockHash, Semaphore[F]]

    override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
      case h: BlockHashMessage =>
        handleBlockHashMessage(peer, h)(
          ignoreCasperMessage
        )
      case b: BlockMessage =>
        handleBlockMessage(b, peer)(
          ignoreCasperMessage,
          blockIsOk,
          sendBlockFromStoreToCasper,
          hashHandlerLock
        )
      case br: BlockRequest => handleBlockRequest(peer, br)
      // TODO should node say it has block only after it is in DAG, or CasperBuffer is enough? Or even just BlockStore?
      // https://github.com/rchain/rchain/pull/2943#discussion_r449887701
      case hbr: HasBlockRequest => handleHasBlockRequest(peer, hbr)(casper.dagContains)
      case hb: HasBlock         => handleHasBlockMessage(peer, hb)(ignoreCasperMessage)
      case _: ForkChoiceTipRequest.type =>
        handleForkChoiceTipRequest(peer)(casper)
      case abr: ApprovedBlockRequest =>
        for {
          approvedBlock <- if (abr.trimState) for {
                            lfBlock <- LastFinalizedStorage[F]
                                        .get(approvedBlock.candidate.block)
                                        .flatMap(BlockStore[F].getUnsafe)
                            lastApprovedBlock = ApprovedBlock(
                              ApprovedBlockCandidate(lfBlock, 0),
                              List.empty
                            )
                          } yield lastApprovedBlock
                          else
                            approvedBlock.pure[F]
          _ <- handleApprovedBlockRequest(peer, approvedBlock)
        } yield ()
      case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable(na.nodeIdentifer)

      // Approved state store records
      case StoreItemsMessageRequest(startPath, skip, take) =>
        handleStateItemsMessageRequest(peer, startPath, skip, take)

      case _ => noop
    }

    override def withCasper[A](
        f: MultiParentCasper[F] => F[A],
        default: F[A]
    ): F[A] = f(casper)
  }
}
