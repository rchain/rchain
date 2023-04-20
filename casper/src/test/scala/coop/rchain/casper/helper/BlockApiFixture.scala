package coop.rchain.casper.helper

import cats.effect.Async
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.api.BlockApiImpl
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.{StatefulExecutionTracker, ValidatorIdentity}
import coop.rchain.comm.rp.Connect.Connection
import coop.rchain.comm.{Endpoint, NodeIdentifier, PeerNode}
import coop.rchain.metrics.Span
import coop.rchain.shared.Log

import scala.collection.compat.immutable.ArraySeq

trait BlockApiFixture {

  def createBlockApi[F[_]: Async: RuntimeManager: BlockDagStorage: BlockStore: Log: Span](
      shardId: String,
      maxDepthLimit: Int,
      validatorIdOpt: Option[ValidatorIdentity] = none
  ): F[BlockApiImpl[F]] = {
    val thisNode = peerNode("testNode", 1234)
    for {
      executionTracker <- StatefulExecutionTracker[F]
      blockApi <- BlockApiImpl[F](
                   validatorOpt = validatorIdOpt,
                   networkId = "rchain",
                   shardId = shardId,
                   minPhloPrice = 1,
                   version = "",
                   (thisNode, List[Connection](), Seq[PeerNode]()).pure[F],
                   isNodeReadOnly = validatorIdOpt.isEmpty,
                   maxDepthLimit = maxDepthLimit,
                   devMode = false,
                   triggerPropose = none,
                   proposerStateRefOpt = none,
                   autoPropose = false,
                   executionTracker = executionTracker
                 )
    } yield blockApi
  }

  def createBlockApi[F[_]: Async](node: TestNode[F]): F[BlockApiImpl[F]] = {
    import node.{blockDagStorage, blockStore, logEff, runtimeManager, sp}

    val thisNode = node.local
    for {
      executionTracker <- StatefulExecutionTracker[F]
      blockApi <- BlockApiImpl[F](
                   validatorOpt = node.validatorIdOpt,
                   networkId = "rchain",
                   shardId = node.shardName,
                   minPhloPrice = 1,
                   version = "",
                   (thisNode, List[Connection](), Seq[PeerNode]()).pure[F],
                   isNodeReadOnly = node.validatorIdOpt.isEmpty,
                   maxDepthLimit = node.apiMaxBlocksLimit,
                   devMode = false,
                   triggerPropose = none,
                   proposerStateRefOpt = none,
                   autoPropose = false,
                   executionTracker = executionTracker
                 )
    } yield blockApi
  }

  protected def endpoint(port: Int): Endpoint = Endpoint("host", port, port)

  protected def peerNode(name: String, port: Int): PeerNode =
    PeerNode(NodeIdentifier(ArraySeq.unsafeWrapArray(name.getBytes)), endpoint(port))
}
