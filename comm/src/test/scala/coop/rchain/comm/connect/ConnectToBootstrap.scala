package coop.rchain.comm.connect

import org.scalatest._
import com.google.common.io.BaseEncoding
import coop.rchain.comm._, CommError._
import coop.rchain.p2p.effects._
import cats._
import coop.rchain.catscontrib._, ski._
import coop.rchain.metrics.Metrics
import coop.rchain.comm.transport._, CommMessages._
import coop.rchain.p2p.EffectsTestInstances._
import scala.concurrent.duration.{Duration, MILLISECONDS}

class ConnectToBootstrapSpec
    extends FunSpec
    with Matchers
    with BeforeAndAfterEach
    with AppendedClues {

  val timeout: Duration = Duration(1, MILLISECONDS)

  type Effect[A] = CommErrT[Id, A]

  val src: PeerNode    = peerNode("src", 30300)
  val remote: PeerNode = peerNode("remote", 30301)

  implicit val logEff            = new LogStub[Effect]
  implicit val timeEff           = new LogicalTime[Effect]
  implicit val metricEff         = new Metrics.MetricsNOP[Effect]
  implicit val nodeDiscoveryEff  = new NodeDiscoveryStub[Effect]()
  implicit val transportLayerEff = new TransportLayerStub[Effect](src)

  override def beforeEach(): Unit = {
    logEff.reset()
    nodeDiscoveryEff.reset()
    transportLayerEff.reset()
  }

  describe("Node when connecting to bootstrap") {
    it("should run attempts before it exits") {
      // given
      transportLayerEff.setResponses(kp(failEverything))
      // when
      val _ = Connect.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5, timeout)
      // then
      logEff.warns should equal(
        List(
          "Failed to connect to bootstrap (attempt 1 / 5)",
          "Failed to connect to bootstrap (attempt 2 / 5)",
          "Failed to connect to bootstrap (attempt 3 / 5)",
          "Failed to connect to bootstrap (attempt 4 / 5)",
          "Failed to connect to bootstrap (attempt 5 / 5)"
        ))
    }

    it("should log on ERROR and return error when failed connecting") {
      // given
      transportLayerEff.setResponses(kp(failEverything))
      // when
      val result =
        Connect.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5, timeout)
      // then
      logEff.errors should equal(List("Failed to connect to bootstrap node, exiting..."))
      result.value should equal(Left(couldNotConnectToBootstrap))
    }

    it("should connect smoothly if there are no issues.") {
      // given
      transportLayerEff.setResponses(kp(alwaysSuccess))
      // when
      val result =
        Connect.connectToBootstrap[Effect](remote.toAddress, maxNumOfAttempts = 5, timeout)
      // then
      logEff.infos should contain(s"Bootstrapping from $remote.")
      logEff.infos should contain(s"Connected $remote.")
      result.value should equal(Right(()))
    }
  }

  // TODO extract common trait for comm tests
  def alwaysSuccess: ProtocolMessage => CommErr[ProtocolMessage] =
    kp(Right(ProtocolHandshakeResponseMessage(protocolHandshake(src))))

  private val failEverything = kp(Left[CommError, ProtocolMessage](unknownProtocol("unknown")))

  private def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  private def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(name.getBytes), endpoint(port))

}
