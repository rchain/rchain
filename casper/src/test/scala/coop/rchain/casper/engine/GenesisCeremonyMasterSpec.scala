package coop.rchain.casper.engine

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.shared.Cell
import monix.eval.Task
import org.scalatest.WordSpec

import scala.concurrent.duration._

class GenesisCeremonyMasterSpec extends WordSpec {
  implicit val traceId: TraceId = Span.next

  "GenesisCeremonyMaster" should {
    "make a transition to Running state after block has been approved" in {
      import monix.execution.Scheduler.Implicits.global
      val fixture = Setup()
      import fixture._

      val requiredSigns = 0
      // interval and duration don't really matter since we don't require and signs from validators
      val interval  = 1.millis
      val duration  = 1.second
      val startTime = System.currentTimeMillis()

      implicit val engineCell = Cell.unsafe[Task, Engine[Task]](Engine.noop)

      def waitUtilCasperIsDefined: Task[MultiParentCasper[Task]] =
        EngineCell[Task].read >>= (_.withCasper(
          casper => Task.pure(casper),
          Task.sleep(3.seconds).flatMap(_ => waitUtilCasperIsDefined)
        ))

      val test = for {
        sigs <- Ref.of[Task, Set[Signature]](Set.empty)
        abp = ApproveBlockProtocol.unsafe[Task](
          genesis,
          requiredSigns,
          duration,
          interval,
          sigs,
          startTime
        )
        _  <- EngineCell[Task].set(new GenesisCeremonyMaster[Task](abp))
        c1 = abp.run().forkAndForget.runToFuture
        c2 = GenesisCeremonyMaster
          .approveBlockInterval[Task](
            interval,
            shardId,
            Some(validatorId),
            traceId
          )
          .forkAndForget
          .runToFuture
        blockApproval = ApproveBlockProtocolTest.approval(
          ApprovedBlockCandidate(Some(genesis), requiredSigns),
          validatorSk,
          validatorPk
        )
        _ <- EngineCell[Task].read >>= (_.handle(local, blockApproval, traceId))
        //wait until casper is defined, with 1 minute timeout (indicating failure)
        possiblyCasper  <- Task.racePair(Task.sleep(1.minute), waitUtilCasperIsDefined)
        _               = assert(possiblyCasper.isRight)
        blockO          <- blockStore.get(genesis.blockHash)
        _               = assert(blockO.isDefined)
        _               = assert(blockO.contains(genesis))
        handlerInternal <- EngineCell[Task].read
        _               = assert(handlerInternal.isInstanceOf[Running[Task]])
        // assert that we really serve last approved block
        lastApprovedBlock <- LastApprovedBlock[Task].get
        _                 = assert(lastApprovedBlock.isDefined)
        _                 <- EngineCell[Task].read >>= (_.handle(local, blockApproval, traceId))
        head              = transportLayer.requests.head
        _ = assert(
          ApprovedBlock
            .parseFrom(head.msg.message.packet.get.content.toByteArray) == lastApprovedBlock.get
        )
      } yield ()

      test.unsafeRunSync
    }
  }
}
