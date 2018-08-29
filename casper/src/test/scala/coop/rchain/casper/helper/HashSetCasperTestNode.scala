package coop.rchain.casper.helper

import java.nio.file.Files

import cats.effect.concurrent.Ref
import cats.{Applicative, ApplicativeError, Id, Monad}
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CasperPacketHandler.{
  ApprovedBlockReceivedHandler,
  CasperPacketHandlerImpl,
  CasperPacketHandlerInternal
}
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.comm.rp.HandleMessages.handle
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Cell
import coop.rchain.shared.PathOps.RichPath
import monix.execution.Scheduler

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.Random
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task

class HashSetCasperTestNode(name: String,
                            val local: PeerNode,
                            tle: TransportLayerTestImpl[Id],
                            val genesis: BlockMessage,
                            sk: Array[Byte],
                            logicalTime: LogicalTime[Id],
                            storageSize: Long = 1024L * 1024,
                            shardId: String = "rchain")(implicit scheduler: Scheduler) {

  import HashSetCasperTestNode.errorHandler

  private val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-$name")

  implicit val logEff            = new LogStub[Id]
  implicit val timeEff           = logicalTime
  implicit val connectionsCell   = Cell.id[Connections](Connect.Connections.empty)
  implicit val transportLayerEff = tle
  implicit val metricEff         = new Metrics.MetricsNOP[Id]
  implicit val errorHandlerEff   = errorHandler
  val dir                        = BlockStoreTestFixture.dbDir
  implicit val blockStore        = BlockStoreTestFixture.create(dir)
  // pre-population removed from internals of Casper
  blockStore.put(genesis.blockHash, genesis)
  implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]
  implicit val rpConfAsk         = createRPConfAsk[Id](local)

  val activeRuntime                  = Runtime.create(storageDirectory, storageSize)
  val runtimeManager                 = RuntimeManager.fromRuntime(activeRuntime)
  val defaultTimeout: FiniteDuration = FiniteDuration(1000, MILLISECONDS)

  val validatorId = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

  val approvedBlock = ApprovedBlock(candidate = Some(ApprovedBlockCandidate(block = Some(genesis))))

  implicit val casperEff =
    MultiParentCasper
      .hashSetCasper[Id](runtimeManager, Some(validatorId), genesis, shardId)

  implicit val multiparentCasperRef = MultiParentCasperRef.unsafe[Id](Some(casperEff))

  implicit val labId = new LastApprovedBlock[Id] {
    private var lab: Option[ApprovedBlock]       = Some(approvedBlock)
    override def get: Id[Option[ApprovedBlock]]  = lab
    override def set(a: ApprovedBlock): Id[Unit] = lab = Some(a)
  }
  val handlerInternal = new ApprovedBlockReceivedHandler(casperEff, approvedBlock)
  val casperPacketHandler = new CasperPacketHandlerImpl[Id](
    Ref.unsafe[Id, CasperPacketHandlerInternal[Id]](handlerInternal))
  implicit val packetHandlerEff = PacketHandler.pf[Id](
    casperPacketHandler.handle
  )

  def receive(): Unit = tle.receive(p => handle[Id](p, defaultTimeout))

  def tearDown(): Unit = {
    tearDownNode()
    dir.recursivelyDelete()
  }

  def tearDownNode(): Unit = {
    activeRuntime.close()
    blockStore.close()
  }
}

object HashSetCasperTestNode {
  def standalone(genesis: BlockMessage, sk: Array[Byte])(
      implicit scheduler: Scheduler): HashSetCasperTestNode = {
    val name     = "standalone"
    val identity = peerNode(name, 40400)
    val tle =
      new TransportLayerTestImpl[Id](identity, Map.empty[PeerNode, mutable.Queue[Protocol]])
    val logicalTime: LogicalTime[Id] = new LogicalTime[Id]

    new HashSetCasperTestNode(name, identity, tle, genesis, sk, logicalTime)
  }

  def network(sks: IndexedSeq[Array[Byte]], genesis: BlockMessage)(
      implicit scheduler: Scheduler): IndexedSeq[HashSetCasperTestNode] = {
    val n                            = sks.length
    val names                        = (1 to n).map(i => s"node-$i")
    val peers                        = names.map(peerNode(_, 40400))
    val msgQueues                    = peers.map(_ -> new mutable.Queue[Protocol]()).toMap
    val logicalTime: LogicalTime[Id] = new LogicalTime[Id]

    val nodes =
      names.zip(peers).zip(sks).map {
        case ((n, p), sk) =>
          val tle = new TransportLayerTestImpl[Id](p, msgQueues)
          new HashSetCasperTestNode(n, p, tle, genesis, sk, logicalTime)
      }

    import Connections._
    //make sure all nodes know about each other
    for {
      n <- nodes
      m <- nodes
      if n.local != m.local
    } {
      n.connectionsCell.modify(_.addConn[Id](m.local)(Monad[Id], n.logEff, n.metricEff))
    }

    nodes
  }

  val appErrId = new ApplicativeError[Id, CommError] {
    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = Applicative[Id].ap[A, B](ff)(fa)
    def pure[A](x: A): Id[A]                       = Applicative[Id].pure[A](x)
    def raiseError[A](e: CommError): Id[A] = {
      val errString = e match {
        case UnknownCommError(msg)                => s"UnknownCommError($msg)"
        case DatagramSizeError(size)              => s"DatagramSizeError($size)"
        case DatagramFramingError(ex)             => s"DatagramFramingError($ex)"
        case DatagramException(ex)                => s"DatagramException($ex)"
        case HeaderNotAvailable                   => "HeaderNotAvailable"
        case ProtocolException(th)                => s"ProtocolException($th)"
        case UnknownProtocolError(msg)            => s"UnknownProtocolError($msg)"
        case PublicKeyNotAvailable(node)          => s"PublicKeyNotAvailable($node)"
        case ParseError(msg)                      => s"ParseError($msg)"
        case EncryptionHandshakeIncorrectlySigned => "EncryptionHandshakeIncorrectlySigned"
        case BootstrapNotProvided                 => "BootstrapNotProvided"
        case PeerNodeNotFound(peer)               => s"PeerNodeNotFound($peer)"
        case PeerUnavailable(peer)                => s"PeerUnavailable($peer)"
        case MalformedMessage(pm)                 => s"MalformedMessage($pm)"
        case CouldNotConnectToBootstrap           => "CouldNotConnectToBootstrap"
        case InternalCommunicationError(msg)      => s"InternalCommunicationError($msg)"
        case TimeOut                              => "TimeOut"
        case _                                    => e.toString
      }

      throw new Exception(errString)
    }

    def handleErrorWith[A](fa: Id[A])(f: (CommError) => Id[A]): Id[A] = fa
  }

  val errorHandler = ApplicativeError_.applicativeError[Id, CommError](appErrId)

  def randomBytes(length: Int): Array[Byte] = Array.fill(length)(Random.nextInt(256).toByte)

  def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
