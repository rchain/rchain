package coop.rchain.casper.engine

import cats.{Applicative, Monad}
import cats.effect.concurrent.Ref
import coop.rchain.casper.{CasperMetricsSource, PrettyPrinter}
import coop.rchain.casper.syntax._
import coop.rchain.comm.PeerNode
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Log, Time}
import cats.syntax.all._
import cats.tagless.autoFunctorK
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.Source

import scala.concurrent.duration.FiniteDuration

/**
  * BlockRetriever makes sure block is received once Casper request it.
  * Block is in scope of BlockRetriever until it is added to CasperBuffer.
  */
@autoFunctorK
trait BlockRetriever[F[_]] {

  /** Make BlocksRetriever process incoming hash
    *
    * @param hash
    * @param peer
    * @param admitHashReason
    * @return
    */
  def admitHash(
      hash: BlockHash,
      peer: Option[PeerNode] = None,
      admitHashReason: BlockRetriever.AdmitHashReason
  ): F[BlockRetriever.AdmitHashResult]

  /**
    * Try to request all pending hashes
    * @param ageThreshold if latest request for hash is older then this value, hash will be requested
    */
  def requestAll(ageThreshold: FiniteDuration): F[Unit]

  /** Acknowledge block receive */
  def ackReceive(hash: BlockHash): F[Unit]

  /** Acknowledge Casper added block */
  def ackInCasper(hash: BlockHash): F[Unit]

  /** If block is received and waiting for Casper to add it */
  def isReceived(hash: BlockHash): F[Boolean]
}

object BlockRetriever {

  trait AdmitHashReason
  final case object HasBlockMessageReceived    extends AdmitHashReason
  final case object HashBroadcastRecieved      extends AdmitHashReason
  final case object MissingDependencyRequested extends AdmitHashReason
  final case object BlockReceived              extends AdmitHashReason

  trait AdmitHashStatus
  final object NewSourcePeerAddedToRequest extends AdmitHashStatus
  final object NewRequestAdded             extends AdmitHashStatus
  final object Ignore                      extends AdmitHashStatus

  final case class AdmitHashResult(
      status: AdmitHashStatus,
      broadcastRequest: Boolean,
      requestBlock: Boolean
  )

  final case class RequestState(
      // Last time block was requested
      timestamp: Long,
      // Peers that were queried for this block
      peers: Set[PeerNode] = Set.empty,
      // If block has been received
      received: Boolean = false,
      // If block has been received
      inCasperBuffer: Boolean = false,
      // Peers that reportedly have block and are yet to be queried
      waitingList: List[PeerNode] = List.empty
  )

  type RequestedBlocks[F[_]] = Ref[F, Map[BlockHash, RequestState]]

  object RequestedBlocks {
    def apply[F[_]: RequestedBlocks]: RequestedBlocks[F] = implicitly[RequestedBlocks[F]]
    def put[F[_]: RequestedBlocks](hash: BlockHash, requested: RequestState): F[Unit] =
      RequestedBlocks[F].update(_ + (hash -> requested))
    def remove[F[_]: RequestedBlocks](hash: BlockHash): F[Unit] =
      RequestedBlocks[F].update(_ - hash)
    def get[F[_]: Monad: RequestedBlocks](hash: BlockHash): F[Option[RequestState]] =
      RequestedBlocks[F].get.map(_.get(hash))
    def contains[F[_]: Monad: RequestedBlocks](hash: BlockHash): F[Boolean] =
      RequestedBlocks[F].get.map(_.contains(hash))
  }

  def apply[F[_]](implicit ev: BlockRetriever[F]): BlockRetriever[F] = ev

  def of[F[_]: Monad: RequestedBlocks: Log: Time: RPConfAsk: TransportLayer: CommUtil: Metrics]
      : BlockRetriever[F] =
    new BlockRetriever[F] {

      private def addSourcePeerToRequest(
          initState: Map[BlockHash, RequestState],
          hash: BlockHash,
          peer: PeerNode
      ): Map[BlockHash, RequestState] =
        initState.get(hash) match {
          case None => initState
          case Some(requestState) =>
            initState + (
              hash ->
                requestState.copy(waitingList = requestState.waitingList ++ List(peer))
            )
        }

      private def addNewRequest(
          initState: Map[BlockHash, RequestState],
          hash: BlockHash,
          now: Long,
          markAsReceived: Boolean = false,
          sourcePeer: Option[PeerNode] = None
      ): Map[BlockHash, RequestState] =
        if (initState.contains(hash)) initState
        else
          initState +
            (hash -> RequestState(
              timestamp = now,
              waitingList = if (sourcePeer.isEmpty) None.toList else List(sourcePeer.get),
              received = markAsReceived
            ))

      /**
        * @param hash            - block hash node encountered
        * @param peer            - peer that node received message with hash from, None hash admit is triggered by some internal
        *                        process, not as a result of external message
        * @param admitHashReason - source of hash info, for logging purposes
        * @return
        */
      override def admitHash(
          hash: BlockHash,
          peer: Option[PeerNode],
          admitHashReason: AdmitHashReason
      ): F[AdmitHashResult] =
        for {
          now <- Time[F].currentMillis
          result <- RequestedBlocks[F]
                     .modify[AdmitHashResult] { state =>
                       val unknownHash = !state.contains(hash)
                       if (unknownHash)
                         (
                           addNewRequest(state, hash, now, sourcePeer = peer),
                           AdmitHashResult(
                             NewRequestAdded,
                             broadcastRequest = peer.isEmpty,
                             requestBlock = peer.nonEmpty
                           )
                         )
                       else {
                         // peer is provided - add it to waiting list
                         if (peer.nonEmpty)
                           (
                             if (state(hash).waitingList.contains(peer.get))
                               (
                                 state,
                                 AdmitHashResult(
                                   Ignore,
                                   broadcastRequest = false,
                                   requestBlock = false
                                 )
                               )
                             else {
                               (
                                 addSourcePeerToRequest(state, hash, peer.get),
                                 AdmitHashResult(
                                   NewSourcePeerAddedToRequest,
                                   broadcastRequest = false,
                                   // if peer is the first one in waiting list - request block from that peer,
                                   // otherwise requests should be triggered by casper loop
                                   requestBlock = state(hash).waitingList.isEmpty
                                 )
                               )
                             }
                           )
                         // otherwise ignore, no new information to add
                         else
                           (
                             state,
                             AdmitHashResult(Ignore, broadcastRequest = false, requestBlock = false)
                           )
                       }
                     }
          _ <- result.status match {
                case NewSourcePeerAddedToRequest =>
                  Log[F]
                    .debug(
                      s"Adding ${peer.get.endpoint.host} to waiting list of ${PrettyPrinter
                        .buildString(hash)} request. " +
                        s"Reason: $admitHashReason"
                    )
                case NewRequestAdded =>
                  Log[F].info(
                    s"Adding ${PrettyPrinter.buildString(hash)} hash to RequestedBlocks because" +
                      s" of $admitHashReason."
                  )
                case Ignore => ().pure[F]
              }
          _ <- if (result.broadcastRequest) CommUtil[F].broadcastHasBlockRequest(hash)
              else ().pure[F]
          _ <- if (result.requestBlock) CommUtil[F].requestForBlock(peer.get, hash)
              else ().pure[F]
        } yield result

      trait AckReceiveResult

      final case object AddedAsReceived extends AckReceiveResult

      final case object MarkedAsReceived extends AckReceiveResult

      override def ackReceive(
          bh: BlockHash
      ): F[Unit] =
        for {
          now <- Time[F].currentMillis
          result <- RequestedBlocks[F].modify[AckReceiveResult] { state =>
                     state.get(bh) match {
                       // There might be blocks that are not maintained by RequestedBlocks, e.g. fork-choice tips
                       case None =>
                         (addNewRequest(state, bh, now, markAsReceived = true), AddedAsReceived)
                       case Some(requested) => {
                         // Make Casper loop aware that the block has been received
                         (state + (bh -> requested.copy(received = true)), MarkedAsReceived)
                       }
                     }
                   }
          _ <- result match {
                case AddedAsReceived =>
                  Log[F].info(
                    s"Block ${PrettyPrinter.buildString(bh)} is not in RequestedBlocks. " +
                      s"Adding and marking recieved."
                  )
                case MarkedAsReceived =>
                  Log[F].info(
                    s"Block ${PrettyPrinter.buildString(bh)} marked as received."
                  )
              }
        } yield ()

      override def ackInCasper(hash: BlockHash): F[Unit] =
        for {
          r <- isReceived(hash)
          _ <- ackReceive(hash).unlessA(r)
          _ <- RequestedBlocks[F].update { state =>
                state + (hash -> state(hash).copy(inCasperBuffer = true))
              }
        } yield ()

      override def isReceived(hash: BlockHash): F[Boolean] =
        RequestedBlocks.get(hash).map(x => x.nonEmpty && x.get.received)

      override def requestAll(
          ageThreshold: FiniteDuration
      ): F[Unit] = {

        def tryRerequest(
            hash: BlockHash,
            requested: RequestState
        ): F[Unit] =
          requested.waitingList match {
            case nextPeer :: waitingListTail =>
              for {
                _ <- Log[F].debug(
                      s"Trying ${nextPeer.endpoint.host} to query for ${PrettyPrinter.buildString(hash)} block. " +
                        s"Remain waiting: ${waitingListTail.map(_.endpoint.host).mkString(", ")}."
                    )
                _  <- CommUtil[F].requestForBlock(nextPeer, hash)
                ts <- Time[F].currentMillis
                _ <- RequestedBlocks.put(
                      hash,
                      requested.copy(
                        timestamp = ts,
                        waitingList = waitingListTail,
                        peers = requested.peers + nextPeer
                      )
                    )
              } yield ()
            case _ =>
              for {
                _ <- Log[F].warn(
                      s"Could not retrieve requested block ${PrettyPrinter.buildString(hash)} " +
                        s"from ${requested.peers.mkString(", ")}. Asking peers again."
                    )
                _ <- RequestedBlocks.remove(hash)
                _ <- CommUtil[F].broadcastHasBlockRequest(hash)
              } yield ()
          }

        import cats.instances.list._
        for {
          state <- RequestedBlocks[F].get
          _ <- Log[F].debug(
                s"Running BlockRetriever maintenance (${state.keys.size} items unexpired)."
              )
          _ <- state.keySet.toList.traverse(hash => {
                val requested = state(hash)
                for {
                  expired <- Time[F].currentMillis
                              .map(_ - requested.timestamp > ageThreshold.toMillis)
                  received = requested.received
                  _ <- Log[F]
                        .debug(
                          s"Casper loop: checking if should re-request " +
                            s"${PrettyPrinter.buildString(hash)}. Received: $received."
                        )
                        .unlessA(received)
                  sentToCasper = requested.inCasperBuffer
                  _            <- tryRerequest(hash, requested).unlessA(received || !expired)
                  // expired added to sentToCasper to mitigate races, which can cause adding block to RequestedBlocks
                  // after they were removed here.
                  _ <- if (sentToCasper && expired) RequestedBlocks.remove(hash) else ().pure[F]
                } yield ()
              })
        } yield ()
      }
    }
}
