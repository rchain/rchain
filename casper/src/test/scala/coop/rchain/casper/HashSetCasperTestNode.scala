package coop.rchain.casper

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.comm._
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.comm.connect.Connect.dispatch
import coop.rchain.comm.transport._
import coop.rchain.comm.protocol.routing._
import java.nio.file.Files

import monix.execution.Scheduler

import scala.collection.mutable
import scala.util.Random

class HashSetCasperTestNode(name: String,
                            val local: PeerNode,
                            tle: TransportLayerTestImpl[Id],
                            genesis: BlockMessage,
                            storageSize: Long = 1024L * 1024)(implicit scheduler: Scheduler) {

  import HashSetCasperTestNode.{errorHandler, peerNode, randomBytes}

  private val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-$name")

  implicit val logEff            = new LogStub[Id]
  implicit val timeEff           = new LogicalTime[Id]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[Id]()
  implicit val transportLayerEff = tle
  implicit val metricEff         = new Metrics.MetricsNOP[Id]
  implicit val errorHandlerEff   = errorHandler
  implicit val turanOracleEffect = SafetyOracle.turanOracle[Id]

  implicit val casperEff =
    MultiParentCasper.hashSetCasper[Id](storageDirectory, storageSize, genesis)

  implicit val packetHandlerEff = PacketHandler.pf[Id](
    casperPacketHandler[Id]
  )

  def receive(): Unit = tle.receive(dispatch[Id] _)

}

object HashSetCasperTestNode {
  def standalone(genesis: BlockMessage)(implicit scheduler: Scheduler): HashSetCasperTestNode = {
    val name     = "standalone"
    val identity = peerNode(name, 30300)
    val tle =
      new TransportLayerTestImpl[Id](identity, Map.empty[PeerNode, mutable.Queue[Protocol]])

    new HashSetCasperTestNode(name, identity, tle, genesis)
  }

  def network(n: Int, genesis: BlockMessage)(
      implicit scheduler: Scheduler): IndexedSeq[HashSetCasperTestNode] = {
    val names     = (1 to n).map(i => s"node-$i")
    val peers     = names.map(peerNode(_, 30300))
    val msgQueues = peers.map(_ -> new mutable.Queue[Protocol]()).toMap

    val nodes =
      names.zip(peers).map {
        case (n, p) =>
          val tle = new TransportLayerTestImpl[Id](p, msgQueues)
          new HashSetCasperTestNode(n, p, tle, genesis)
      }

    //make sure all nodes know about each other
    for {
      n <- nodes
      m <- nodes
      if n.local != m.local
    } {
      n.nodeDiscoveryEff.addNode(m.local)
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
        case MalformedMessage(pm)                 => s"MalformedMessage($pm)"
        case CouldNotConnectToBootstrap           => "CouldNotConnectToBootstrap"
        case InternalCommunicationError(msg)      => s"InternalCommunicationError($msg)"
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
