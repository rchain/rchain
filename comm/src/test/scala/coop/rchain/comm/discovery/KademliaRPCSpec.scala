package coop.rchain.comm.discovery

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.{NodeIdentifier, PeerNode}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.util.Random
import cats.effect.kernel.Async

abstract class KademliaRPCSpec[F[_]: Async, E <: Environment]
    extends KademliaRPCRuntime[F, E]
    with AnyWordSpecLike
    with Matchers {

  val kademliaRpcName: String = this.getClass.getSimpleName.replace("Spec", "")

  kademliaRpcName when {
    "pinging a remote peer" when {
      "everything is fine" should {
        "send and receive a positive response" in
          new TwoNodesRuntime[Boolean]() {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Boolean] = kademliaRPC.ping(remote)

            val result: TwoNodesResult = run()

            result() shouldEqual true
            pingHandler.received should have length 1
            val (receiver, sender) = pingHandler.received.head
            receiver shouldEqual result.remoteNode
            sender shouldEqual result.localNode
          }

        "send twice and receive positive responses" in
          new TwoNodesRuntime[(Boolean, Boolean)]() {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[(Boolean, Boolean)] =
              for {
                r1 <- kademliaRPC.ping(remote)
                r2 <- kademliaRPC.ping(remote)
              } yield (r1, r2)

            val result: TwoNodesResult = run()

            result() shouldEqual (true, true)
            pingHandler.received should have length 2
            val (receiver1, sender1) = pingHandler.received.head
            val (receiver2, sender2) = pingHandler.received.tail.head
            receiver1 shouldEqual result.remoteNode
            receiver2 shouldEqual result.remoteNode
            sender1 shouldEqual result.localNode
            sender2 shouldEqual result.localNode
          }
      }

      "response takes to long" should {
        "get a negative result" in
          new TwoNodesRuntime[Boolean](
            pingHandler = Handler.pingHandlerWithDelay(1.second)
          ) {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Boolean] = kademliaRPC.ping(remote)

            val result: TwoNodesResult = run()

            result() shouldEqual false
            pingHandler.received should have length 1
            val (receiver, sender) = pingHandler.received.head
            receiver shouldEqual result.remoteNode
            sender shouldEqual result.localNode
          }
      }

      "peer is not listening" should {
        "get a negative result" in
          new TwoNodesRemoteDeadRuntime[Boolean]() {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Boolean] = kademliaRPC.ping(remote)

            val result: TwoNodesResult = run()

            result() shouldEqual false
          }
      }
    }

    "doing a lookup to a remote peer" when {
      val key = Array.ofDim[Byte](40)
      Random.nextBytes(key)
      val otherPeer = PeerNode.from(NodeIdentifier(key), "1.2.3.4", 0, 0)

      "everything is fine" should {
        "send and receive a list of peers" in
          new TwoNodesRuntime[Seq[PeerNode]](
            lookupHandler = Handler.lookupHandler(Seq(otherPeer))
          ) {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Seq[PeerNode]] = kademliaRPC.lookup(key, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Seq(otherPeer)
            lookupHandler.received should have length 1
            val (receiver, (sender, k)) = lookupHandler.received.head
            receiver shouldEqual result.remoteNode
            sender shouldEqual result.localNode
            k should be(key)
          }
      }

      "response takes to long" should {
        "get an empty list result" in
          new TwoNodesRuntime[Seq[PeerNode]](
            lookupHandler = Handler.lookupHandlerWithDelay(1.second)
          ) {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Seq[PeerNode]] = kademliaRPC.lookup(key, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Seq.empty[PeerNode]
            lookupHandler.received should have length 1
            val (receiver, (sender, k)) = lookupHandler.received.head
            receiver shouldEqual result.remoteNode
            sender shouldEqual result.localNode
            k should be(key)
          }
      }

      "peer is not listening" should {
        "get an empty list result" in
          new TwoNodesRemoteDeadRuntime[Seq[PeerNode]]() {
            def execute(
                kademliaRPC: KademliaRPC[F],
                local: PeerNode,
                remote: PeerNode
            ): F[Seq[PeerNode]] = kademliaRPC.lookup(key, remote)

            val result: TwoNodesResult = run()

            result() shouldEqual Seq.empty[PeerNode]
          }
      }
    }
  }
}
