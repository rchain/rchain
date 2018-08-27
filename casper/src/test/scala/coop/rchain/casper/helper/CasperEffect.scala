package coop.rchain.casper.helper

import cats.data.EitherT
import coop.rchain.blockstorage.LMDBBlockStore
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.TransportLayerTestImpl
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{MultiParentCasper, SafetyOracle, ValidatorIdentity}
import coop.rchain.catscontrib._
import coop.rchain.comm._
import coop.rchain.comm.protocol.routing._
import coop.rchain.comm.rp.Connect
import coop.rchain.comm.rp.Connect._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.metrics.Metrics
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.shared._
import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.mutable

object CasperEffect {
  type Effect[A] = EitherT[Task, CommError, A]

  def apply(sk: Array[Byte], genesis: BlockMessage, shardId: String = "rchain")(
      implicit scheduler: Scheduler)
    : (Effect[MultiParentCasper[Effect]], () => Unit, LogStub[Effect]) = {
    val blockStoreDir            = BlockStoreTestFixture.dbDir
    val runtimeDir               = BlockStoreTestFixture.dbDir
    val local                    = HashSetCasperTestNode.peerNode("taskNode", 40400)
    val log                      = new LogStub[Effect]
    implicit val logEff          = log
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
      _ <- blockStoreEff.put(genesis.blockHash, genesis)
      hashSetCasper <- MultiParentCasper
                        .hashSetCasper[Effect](runtimeManager, Some(validatorId), genesis, shardId)
    } yield hashSetCasper

    def cleanUp(): Unit = {
      activeRuntime.close()
      blockStoreEff.close()
      runtimeDir.recursivelyDelete()
      blockStoreDir.recursivelyDelete()
    }

    (casperTask, cleanUp _, log)
  }

  private val syncInstance = SyncInstances.syncEffect[CommError](commError => {
    new Exception(s"CommError: $commError")
  }, e => { UnknownCommError(e.getMessage) })
}
