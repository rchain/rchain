package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift}
import cats._, cats.data._, cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.{BlockDagRepresentation, InMemBlockDagStorage, InMemBlockStore}
import coop.rchain.casper.MultiParentCasperTestUtil.createBonds
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.engine._, EngineCell._
import coop.rchain.casper.helper.{BlockDagStorageTestFixture, NoOpsCasperEffect}
import coop.rchain.casper.protocol.{NoApprovedBlockAvailable, _}
import coop.rchain.casper.util.TestTime
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.catscontrib.ApplicativeError_
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.rp.Connect.{Connections, ConnectionsCell}
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.{transport, _}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.{Cell, StoreType}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.WordSpec

import scala.concurrent.duration._

class GenesisValidatorSpec extends WordSpec {

  "GenesisCeremonyMaster" should {
    "respond on UnapprovedBlock messages with BlockApproval" in {
      implicit val ctx = Scheduler.global
      val fixture      = Setup()
      import fixture._

      implicit val engineCell: EngineCell[Task] =
        Cell.unsafe[Task, Engine[Task]](Engine.noop)
      val expectedCandidate = ApprovedBlockCandidate(Some(genesis), requiredSigs)
      val unapprovedBlock   = BlockApproverProtocolTest.createUnapproved(requiredSigs, genesis)
      val test = for {
        _ <- engineCell.set(
              new GenesisValidator(runtimeManager, validatorId, shardId, bap)
            )
        _             <- engineCell.read >>= (_.handle(local, unapprovedBlock))
        blockApproval = BlockApproverProtocol.getBlockApproval(expectedCandidate, validatorId)
        expectedPacket = ProtocolHelper.packet(
          local,
          networkId,
          transport.BlockApproval,
          blockApproval.toByteString
        )
        _ = {
          val lastMessage = transportLayer.requests.last
          assert(lastMessage.peer == local && lastMessage.msg == expectedPacket)
        }
      } yield ()
      test.unsafeRunSync
    }

    "should not respond to any other message" in {
      implicit val ctx = Scheduler.global
      val fixture      = Setup()
      import fixture._

      implicit val engineCell: EngineCell[Task] =
        Cell.unsafe[Task, Engine[Task]](Engine.noop)

      val approvedBlockRequest = ApprovedBlockRequest("test")
      val test = for {
        _ <- engineCell.set(
              new GenesisValidator(runtimeManager, validatorId, shardId, bap)
            )
        _    <- engineCell.read >>= (_.handle(local, approvedBlockRequest))
        head = transportLayer.requests.head
        response = packet(
          local,
          networkId,
          transport.NoApprovedBlockAvailable,
          NoApprovedBlockAvailable(approvedBlockRequest.identifier, local.toString).toByteString
        )
        _            = assert(head.peer == local && head.msg == response)
        _            = transportLayer.reset()
        blockRequest = BlockRequest("base16Hash", ByteString.copyFromUtf8("base16Hash"))
        _            <- engineCell.read >>= (_.handle(local, blockRequest))
        _            = assert(transportLayer.requests.isEmpty)
      } yield ()
      test.unsafeRunSync
    }
  }

}
