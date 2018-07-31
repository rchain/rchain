package coop.rchain.casper.helper

import cats._
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
import coop.rchain.comm.connect.Connect.dispatch
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

object TaskCasper {
  def apply(sk: Array[Byte], genesis: BlockMessage)(
      implicit scheduler: Scheduler): (Task[MultiParentCasper[Task]], () => Unit) = {
    val blockStoreDir = BlockStoreTestFixture.dbDir
    val runtimeDir    = BlockStoreTestFixture.dbDir

    implicit val logEff           = new LogStub[Task]
    implicit val timeEff          = new LogicalTime[Task]
    implicit val errorHandlerEff  = ApplicativeError_.applicativeError[Task, CommError](appErrTask)
    implicit val nodeDiscoveryEff = new NodeDiscoveryStub[Task]()
    implicit val transportLayerEff =
      new TransportLayerTestImpl[Task](HashSetCasperTestNode.peerNode("taskNode", 40400),
                                       Map.empty[PeerNode, mutable.Queue[Protocol]])
    implicit val metricEff = new Metrics.MetricsNOP[Task]
    implicit val blockStoreEff =
      LMDBBlockStore.create[Task](LMDBBlockStore.Config(blockStoreDir, 1024L * 1024))(syncInstance,
                                                                                      metricEff)
    implicit val turanOracleEffect = SafetyOracle.turanOracle[Task]

    val activeRuntime  = Runtime.create(runtimeDir, 1024L * 1024)
    val runtimeManager = RuntimeManager.fromRuntime(activeRuntime)
    val validatorId    = ValidatorIdentity(Ed25519.toPublic(sk), sk, "ed25519")

    val casperTask = for {
      _        <- blockStoreEff.put(genesis.blockHash, genesis)
      blockMap <- blockStoreEff.asMap()
    } yield
      MultiParentCasper.hashSetCasper[Task](runtimeManager, Some(validatorId), genesis, blockMap)

    def cleanUp(): Unit = {
      activeRuntime.close()
      blockStoreEff.close()
      runtimeDir.recursivelyDelete()
      blockStoreDir.recursivelyDelete()
    }

    (casperTask, cleanUp _)
  }

  private val appErrTask = new ApplicativeError[Task, CommError] {
    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      ApplicativeError[Task, Throwable].ap[A, B](ff)(fa)
    def pure[A](x: A): Task[A] = ApplicativeError[Task, Throwable].pure[A](x)
    def raiseError[A](e: CommError): Task[A] =
      ApplicativeError[Task, Throwable].raiseError(new Exception(e.toString))

    def handleErrorWith[A](fa: Task[A])(f: (CommError) => Task[A]): Task[A] = fa
  }

  private val syncInstance =
    new Sync[Task] {
      def suspend[A](thunk: => Task[A]): Task[A] = Task.defer(thunk)

      def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
        implicitly[FlatMap[Task]].flatMap(fa)(f)

      def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
        implicitly[FlatMap[Task]].tailRecM(a)(f)

      def raiseError[A](e: Throwable): Task[A] = Task.raiseError(e)

      def pure[A](x: A): Task[A] = implicitly[Applicative[Task]].pure(x)

      def bracketCase[A, B](acquire: Task[A])(use: A => Task[B])(
          release: (A, ExitCase[Throwable]) => Task[Unit]): Task[B] =
        acquire flatMap { state =>
          try {
            use(state)
          } catch {
            case t: Throwable => raiseError(t)
          } finally {
            release(state, ExitCase.Completed)
          }
        }

      def handleErrorWith[A](fa: Task[A])(f: Throwable => Task[A]): Task[A] =
        ApplicativeError[Task, Throwable].handleErrorWith(fa)(f)
    }

}
