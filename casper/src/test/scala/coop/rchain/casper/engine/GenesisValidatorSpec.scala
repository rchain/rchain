package coop.rchain.casper.engine

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol.{NoApprovedBlockAvailable, _}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.casper.helper.RSpaceStateManagerTestImpl
import coop.rchain.shared.{Cell, EventPublisher}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.WordSpec

class GenesisValidatorSpec extends WordSpec {
  implicit val eventBus = EventPublisher.noop[Task]

  "GenesisCeremonyMaster" should {
    "respond on UnapprovedBlock messages with BlockApproval" in {
      val fixture = Setup()
      import fixture._

      implicit val engineCell: EngineCell[Task] =
        Cell.unsafe[Task, Engine[Task]](Engine.noop)
      implicit val rspaceMan = RSpaceStateManagerTestImpl[Task]()

      val expectedCandidate = ApprovedBlockCandidate(genesis, requiredSigs)
      val unapprovedBlock   = BlockApproverProtocolTest.createUnapproved(requiredSigs, genesis)
      val test = for {
        _ <- engineCell.set(
              new GenesisValidator(validatorId, shardId, finalizationRate, bap)
            )
        _             <- engineCell.read >>= (_.handle(local, unapprovedBlock))
        blockApproval = BlockApproverProtocol.getBlockApproval(expectedCandidate, validatorId)
        expectedPacket = ProtocolHelper.packet(
          local,
          networkId,
          blockApproval.toProto
        )
        _ = {
          val lastMessage = transportLayer.requests.last
          assert(lastMessage.peer == local && lastMessage.msg == expectedPacket)
        }
      } yield ()
      test.unsafeRunSync
    }

    "should not respond to any other message" in {
      val fixture = Setup()
      import fixture._

      implicit val engineCell: EngineCell[Task] =
        Cell.unsafe[Task, Engine[Task]](Engine.noop)
      implicit val rspaceMan = RSpaceStateManagerTestImpl[Task]()

      val approvedBlockRequest = ApprovedBlockRequest("test")
      val test = for {
        _ <- engineCell.set(
              new GenesisValidator(validatorId, shardId, finalizationRate, bap)
            )
        _    <- engineCell.read >>= (_.handle(local, approvedBlockRequest))
        head = transportLayer.requests.head
        response = packet(
          local,
          networkId,
          NoApprovedBlockAvailable(approvedBlockRequest.identifier, local.toString).toProto
        )
        _            = assert(head.peer == local && head.msg == response)
        _            = transportLayer.reset()
        blockRequest = BlockRequest(ByteString.copyFromUtf8("base16Hash"))
        _            <- engineCell.read >>= (_.handle(local, blockRequest))
        _            = assert(transportLayer.requests.isEmpty)
      } yield ()
      test.unsafeRunSync
    }
  }

}
