package coop.rchain.casper.engine

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.ski._
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.rp.ProtocolHelper._
import coop.rchain.comm.transport
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.shared.Cell
import monix.eval.Task
import org.scalatest.WordSpec

class InitializingSpec extends WordSpec {
  implicit val traceId: TraceId = Span.next

  "Initializing state" should {
    "make a transition to Running once ApprovedBlock has been received" in {
      import monix.execution.Scheduler.Implicits.global
      val fixture = Setup()
      import fixture._

      val validators = Set(ByteString.copyFrom(validatorPk.bytes))

      val theInit = Task.unit

      implicit val engineCell = Cell.unsafe[Task, Engine[Task]](Engine.noop)

      // interval and duration don't really matter since we don't require and signs from validators
      val initializingEngine =
        new Initializing[Task](
          shardId,
          Some(validatorId),
          validators,
          theInit
        )

      val approvedBlockCandidate = ApprovedBlockCandidate(block = Some(genesis))

      val approvedBlock: ApprovedBlock = ApprovedBlock(
        candidate = Some(approvedBlockCandidate),
        sigs = Seq(
          Signature(
            ByteString.copyFrom(validatorPk.bytes),
            "secp256k1",
            ByteString.copyFrom(
              Secp256k1.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
            )
          )
        )
      )

      val test = for {
        _               <- EngineCell[Task].set(initializingEngine)
        _               <- initializingEngine.handle(local, approvedBlock, traceId)
        engine          <- EngineCell[Task].read
        casperDefined   <- engine.withCasper(kp(true.pure[Task]), false.pure[Task])
        _               = assert(casperDefined)
        blockO          <- blockStore.get(genesis.blockHash)
        _               = assert(blockO.isDefined)
        _               = assert(blockO.contains(genesis))
        handlerInternal <- EngineCell[Task].read
        _               = assert(handlerInternal.isInstanceOf[Running[Task]])
        _ = assert(
          transportLayer.requests.head.msg == packet(
            local,
            networkId,
            transport.ForkChoiceTipRequest,
            ByteString.EMPTY
          )
        )
        _ = transportLayer.reset()
        // assert that we really serve last approved block
        lastApprovedBlockO <- LastApprovedBlock[Task].get
        _                  = assert(lastApprovedBlockO.isDefined)
        _                  <- EngineCell[Task].read >>= (_.handle(local, ApprovedBlockRequest("test"), traceId))
        head               = transportLayer.requests.head
        _                  = assert(head.msg.message.packet.get.content == approvedBlock.toByteString)
      } yield ()

      test.unsafeRunSync
    }
  }

}
