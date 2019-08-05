package coop.rchain.casper.engine

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.engine.EngineCell._
import coop.rchain.casper.protocol.{NoApprovedBlockAvailable, _}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.rp.ProtocolHelper
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.shared.Cell
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.WordSpec

class GenesisValidatorSpec extends WordSpec {
  implicit val traceId: TraceId = Span.next

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
              new GenesisValidator(validatorId, shardId, bap)
            )
        _             <- engineCell.read >>= (_.handle(local, unapprovedBlock, traceId))
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
              new GenesisValidator(validatorId, shardId, bap)
            )
        _    <- engineCell.read >>= (_.handle(local, approvedBlockRequest, traceId))
        head = transportLayer.requests.head
        response = packet(
          local,
          networkId,
          transport.NoApprovedBlockAvailable,
          NoApprovedBlockAvailable(approvedBlockRequest.identifier, local.toString).toByteString
        )
        _            = assert(head.peer == local && head.msg == response)
        _            = transportLayer.reset()
        blockRequest = BlockRequest(ByteString.copyFromUtf8("base16Hash"))
        _            <- engineCell.read >>= (_.handle(local, blockRequest, traceId))
        _            = assert(transportLayer.requests.isEmpty)
      } yield ()
      test.unsafeRunSync
    }
  }

}
