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
import monix.eval.Task
import monix.execution.Scheduler
import coop.rchain.models.BlockHash.BlockHash
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** Node in this state has already received at least one [[ApprovedBlock]] and it has created an instance
  * of [[MultiParentCasper]].
  *
  * In the future it will be possible to create checkpoint with new [[ApprovedBlock]].
  **/
object Running {

  final case class Requested()
  type RequestedBlocks[F[_]] = Cell[F, Map[BlockHash, Requested]]
  object RequestedBlocks {
    def apply[F[_]: RequestedBlocks]: RequestedBlocks[F] = implicitly[RequestedBlocks[F]]
  }

  def noop[F[_]: Applicative]: F[Unit] = ().pure[F]

  /** TODO
    * - check if was requested already
    *    - if requested for same peer, ignore
         - if requested for different peer, store this peer on waiting list (remove duplicates or use set)
    * - periodically check the lists of requests and checks if we waited to long
    *    - if waited to long and there is at least onee peer on waiting list:
    *        - request the block from that peer from waiting list
    *        - move the peer from the waiting list to the requested list
    *    - if the waiting list is empty, log a warning (this means most likely things are not going the way they should)
    * - upon handling the BlockMessage
    *    - if the block was added to casper - remove its entry in the requests list
    *    - if the block was not added, keep the list
    */
  def handleHasBlock[F[_]: Monad: RPConfAsk: RequestedBlocks: TransportLayer](
      peer: PeerNode,
      hb: HasBlock
  )(
      casperContains: BlockHash => F[Boolean]
  ): F[Unit] =
    casperContains(hb.hash).ifM(
      noop[F],
      for {
        conf <- RPConfAsk[F].ask
        msg = packet(
          conf.local,
          conf.networkId,
          transport.BlockRequest,
          BlockRequest(hb.hash).toByteString
        )
        _ <- TransportLayer[F].send(peer, msg)
        _ <- RequestedBlocks[F].modify(_ + (hb.hash -> Requested()))
      } yield ()
    )
}

class Running[F[_]: RPConfAsk: BlockStore: Monad: ConnectionsCell: TransportLayer: Log: Time: Running.RequestedBlocks](
    casper: MultiParentCasper[F],
    approvedBlock: ApprovedBlock,
    theInit: F[Unit]
) extends Engine[F] {
  import Engine._

  def applicative: Applicative[F] = Applicative[F]

  override def init: F[Unit] = theInit

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

  override def handle(peer: PeerNode, msg: CasperMessage): F[Unit] = msg match {
    case b: BlockMessage              => handleBlockMessage(peer, b)
    case br: BlockRequest             => handleBlockRequest(peer, br)
    case hbr: HasBlockRequest         => handleHasBlockRequest(peer, hbr)
    case hb: HasBlock                 => Running.handleHasBlock[F](peer, hb)(casper.contains)
    case fctr: ForkChoiceTipRequest   => handleForkChoiceTipRequest(peer, fctr)
    case abr: ApprovedBlockRequest    => handleApprovedBlockRequest(peer, abr)
    case na: NoApprovedBlockAvailable => logNoApprovedBlockAvailable[F](na.nodeIdentifer)
    case _                            => noop
  }

  private def handleBlockMessage(peer: PeerNode, b: BlockMessage): F[Unit] =
    casper
      .contains(b.blockHash)
      .ifM(
        Log[F].info(s"Received block ${PrettyPrinter.buildString(b.blockHash)} again."),
        for {
          _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(b)}.")
          _ <- casper.addBlock(b, handleDoppelganger(peer, _, _))
        } yield ()
      )

  private def handleHasBlockRequest(peer: PeerNode, hbr: HasBlockRequest): F[Unit] =
    BlockStore[F].get(hbr.hash) >>= {
      case Some(_) =>
        for {
          conf <- RPConfAsk[F].ask
          msg = packet(
            conf.local,
            conf.networkId,
            transport.HasBlock,
            HasBlock(hbr.hash).toByteString
          )
          _ <- TransportLayer[F].send(peer, msg)
        } yield ()
      case None => noop
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
