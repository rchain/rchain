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

class InitializingSpec extends WordSpec {
  "Initializing state" should {
    "make a transition to Running once ApprovedBlock has been received" in {
      import monix.execution.Scheduler.Implicits.global
      val fixture = Setup()
      import fixture._

      val validators = Set(ByteString.copyFrom(validatorPk.bytes))

      val theInit = Task.unit

      implicit val engineCell = Cell.unsafe[Task, Engine[Task]](Engine.noop)

      // interval and duration don't really matter since we don't require and signs from validators
      val bootstrapCasper =
        new Initializing[Task](
          runtimeManager,
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
            "ed25519",
            ByteString.copyFrom(
              Ed25519.sign(Blake2b256.hash(approvedBlockCandidate.toByteArray), validatorSk)
            )
          )
        )
      )

      val test = for {
        _               <- EngineCell[Task].set(bootstrapCasper)
        _               <- bootstrapCasper.handle(local, approvedBlock)
        casperO         <- MultiParentCasperRef[Task].get
        _               = assert(casperO.isDefined)
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
        _                  <- EngineCell[Task].read >>= (_.handle(local, ApprovedBlockRequest("test")))
        head               = transportLayer.requests.head
        _                  = assert(head.msg.message.packet.get.content == approvedBlock.toByteString)
      } yield ()

      test.unsafeRunSync
    }
  }

}
