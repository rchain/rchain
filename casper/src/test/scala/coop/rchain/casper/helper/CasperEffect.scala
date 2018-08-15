package coop.rchain.casper.helper

import coop.rchain.comm.rp.Connect, Connect._
import coop.rchain.shared._
import cats.{Applicative, ApplicativeError, FlatMap}
import cats.data.EitherT
import cats.effect.{ExitCase, Sync}
import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil.casperPacketHandler
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.{
  MultiParentCasper,
  MultiParentCasperConstructor,
  SafetyOracle,
  ValidatorIdentity
}
import coop.rchain.catscontrib._
import coop.rchain.comm._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.p2p.effects.PacketHandler
import coop.rchain.comm.protocol.routing._
import coop.rchain.rholang.interpreter.Runtime
import java.nio.file.Files

import coop.rchain.casper.util.rholang.RuntimeManager
import monix.execution.Scheduler
import monix.eval.Task
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.collection.mutable
import coop.rchain.shared.PathOps.RichPath
import scala.util.Random

object CasperEffect {
  type Effect[A] = EitherT[Task, CommError, A]

  def apply(sk: Array[Byte], genesis: BlockMessage, shardId: String = "rchain")(
      implicit scheduler: Scheduler): (Effect[MultiParentCasper[Effect]], () => Unit) = {
    val blockStoreDir            = BlockStoreTestFixture.dbDir
    val runtimeDir               = BlockStoreTestFixture.dbDir
    val local                    = HashSetCasperTestNode.peerNode("taskNode", 40400)
    implicit val logEff          = new LogStub[Effect]
    implicit val timeEff         = new LogicalTime[Effect]
    implicit val connectionsCell = Cell.const[Effect, Connections](Connect.Connections.empty)
    implicit val transportLayerEff =
      new TransportLayerTestImpl[Effect](local, Map.empty[PeerNode, mutable.Queue[Protocol]])
    implicit val metricEff = new Metrics.MetricsNOP[Effect]
    implicit val blockStoreEff =
      LMDBBlockStore.create[Effect](LMDBBlockStore.Config(blockStoreDir, 1024L * 1024))(
        syncInstance,
        metricEff)
    implicit val turanOracleEffect = SafetyOracle.turanOracle[Effect]
    implicit val rpConfAsk         = createRPConfAsk[Effect](local)

    val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
    val validatorId    = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

    val casperTask = for {
      _        <- blockStoreEff.put(genesis.blockHash, genesis)
      blockMap <- blockStoreEff.asMap()
    } yield
      MultiParentCasper.hashSetCasper[Effect](
        runtimeManager,
        Some(validatorId),
        genesis,
        blockMap,
        shardId
      )

    def cleanUp(): Unit = {
      activeRuntime.close()
      blockStoreEff.close()
      runtimeDir.recursivelyDelete()
      blockStoreDir.recursivelyDelete()
    }

    (casperTask, cleanUp _)
  }

  private val syncInstance = SyncInstances.syncEffect[CommError](commError => {
    new Exception(s"CommError: $commError")
  }, e => { UnknownCommError(e.getMessage) })
}
