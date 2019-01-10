package coop.rchain.node.effects
import cats.Monad
import coop.rchain.node.TaskEffectOps
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{Bond, Deploy}
import coop.rchain.casper.util.rholang.{Failed, InternalProcessedDeploy, RuntimeManager}
import coop.rchain.models.{BindPattern, Par}
import coop.rchain.node.Effect
import monix.eval.Task

object EitherTRuntimeManager {
  def eitherTRuntimeManager(
      value: Monad[Task],
      runtimeManager: RuntimeManager[Task]
  ): RuntimeManager[Effect] =
    new RuntimeManager[Effect]() {
      override def captureResults(
          start: RuntimeManager.StateHash,
          deploy: Deploy,
          name: String
      ): Effect[scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).toEffect

      override def captureResults(
          start: RuntimeManager.StateHash,
          deploy: Deploy,
          name: Par
      ): Effect[scala.Seq[Par]] = runtimeManager.captureResults(start, deploy, name).toEffect

      override def replayComputeState(
          hash: RuntimeManager.StateHash,
          terms: scala.Seq[InternalProcessedDeploy],
          time: Option[Long]
      ): Effect[scala.Either[(Option[Deploy], Failed), RuntimeManager.StateHash]] =
        runtimeManager.replayComputeState(hash, terms, time).toEffect

      override def computeState(
          hash: RuntimeManager.StateHash,
          terms: scala.Seq[Deploy],
          time: Option[Long]
      ): Effect[(RuntimeManager.StateHash, scala.Seq[InternalProcessedDeploy])] =
        runtimeManager.computeState(hash, terms, time).toEffect

      override def storageRepr(
          hash: RuntimeManager.StateHash
      ): Effect[Option[String]] = runtimeManager.storageRepr(hash).toEffect

      override def computeBonds(
          hash: RuntimeManager.StateHash
      ): Effect[scala.Seq[Bond]] = runtimeManager.computeBonds(hash).toEffect

      override def getData(
          hash: ByteString,
          channel: Par
      ): Effect[scala.Seq[Par]] = runtimeManager.getData(hash, channel).toEffect

      override def getContinuation(
          hash: ByteString,
          channels: scala.collection.immutable.Seq[Par]
      ): Effect[scala.Seq[(scala.Seq[BindPattern], Par)]] =
        runtimeManager.getContinuation(hash, channels).toEffect

      override def emptyStateHash: BlockHash = runtimeManager.emptyStateHash
    }
}
