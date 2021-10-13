package coop.rchain.node.mergeablity

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.merging.{BlockIndexer, DeployChainIndex}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager

trait ComputeMerge {

  /**
    * The cases below are always following the pattern below.
    *     MergedBlock
    *   /           \
    * LeftBlock   RightBlock
    *   \           /
    *     BaseBlock
    *
    * `BaseBlock` is the start point of two branches which is trying to merge.
    * `LeftBlock` is the main state which would be take all the state in the merged block.
    * `RightBlock` is the block which is trying to merge into `LeftBlock`. If there is anything in `RightBlock` conflict
    * with `LeftBlock`, it would be rejected.
    *
    *           MergedBlock
    *       /                   \
    * B2 deploy1 "@0!(1)"      B3  Seq(deploy2("@0!(0)") , deploy3("@0!(22)"))
    *       \                   /
    *        B1  "contract @0(0) = { 0 } | for (@1 <- @0) { 0 }"
    *
    */
  def computeMergeCase[F[_]: Concurrent: Span: Log: Metrics: Parallel: ContextShift](
      baseDeploySources: Seq[Signed[DeployData]],
      leftDeploySources: Seq[Signed[DeployData]],
      rightDeploySources: Seq[Signed[DeployData]],
      checkFunction: (
          RhoRuntime[F],
          RhoHistoryRepository[F],
          (Blake2b256Hash, Seq[ByteString])
      ) => F[Unit],
      rejectRight: Boolean // false reject left, true reject right
  ): F[Unit] = {
    case class MergingNode(index: BlockIndex, isFinalized: Boolean, postState: Blake2b256Hash)

    rhoRuntimeEff[F](true)
      .use {
        case (runtime, _, historyRepo) =>
          for {
            baseDeploysRes <- baseDeploySources.toList.traverse(
                               runtime.processDeployWithMergeableData
                             )
            (baseDeploys, baseMergeChs) = baseDeploysRes.unzip
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${baseDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(baseDeploys.exists(_.isFailed))
            baseCheckpoint <- runtime.createCheckpoint
            leftDeploysRes <- leftDeploySources.toList.traverse(
                               runtime.processDeployWithMergeableData
                             )
            (leftDeploys, leftMergeChs) = leftDeploysRes.unzip
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${leftDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(leftDeploys.exists(_.isFailed))
            leftCheckpoint @ _ <- runtime.createCheckpoint
            _                  <- runtime.reset(baseCheckpoint.root)
            rightDeploysRes <- rightDeploySources.toList.traverse(
                                runtime.processDeployWithMergeableData
                              )
            (rightDeploys, rightMergeChs) = rightDeploysRes.unzip
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${rightDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(rightDeploys.exists(_.isFailed))
            rightCheckpoint @ _ <- runtime.createCheckpoint

            leftIndex <- BlockIndexer(
                          ByteString.copyFromUtf8("l"),
                          leftDeploys,
                          List.empty,
                          baseCheckpoint.root,
                          leftCheckpoint.root,
                          historyRepo,
                          leftMergeChs
                        )
            rightIndex <- BlockIndexer(
                           ByteString.copyFromUtf8("r"),
                           rightDeploys,
                           List.empty,
                           baseCheckpoint.root,
                           rightCheckpoint.root,
                           historyRepo,
                           rightMergeChs
                         )
            baseIndex <- BlockIndexer(
                          ByteString.EMPTY,
                          List.empty,
                          List.empty,
                          baseCheckpoint.root, // this does not matter
                          baseCheckpoint.root,
                          historyRepo,
                          Seq.empty
                        )
            kvm      = new InMemoryStoreManager
            dagStore <- BlockDagKeyValueStorage.create[F](kvm)
            bBlock = getRandomBlock(
              setPreStateHash = RuntimeManager.emptyStateHashFixed.some,
              setPostStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List.empty.some
            )
            rBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(rightCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List(bBlock.blockHash).some
            )
            lBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(leftCheckpoint.root.bytes.toArray).some,
              setParentsHashList = List(bBlock.blockHash).some
            )
            _   <- dagStore.insert(bBlock, false, approved = true)
            _   <- dagStore.insert(lBlock, false)
            _   <- dagStore.insert(rBlock, false)
            dag <- dagStore.getRepresentation
            indices = Map(
              bBlock.blockHash -> baseIndex,
              rBlock.blockHash -> rightIndex,
              lBlock.blockHash -> leftIndex
            )
            rejectAlg = (r: DeployChainIndex) => {
              val deployIds      = r.deploysWithCost.map(_.id)
              val rightDeployIds = rightDeploys.map(_.deploy.sig).toSet
              val leftDeployIds  = leftDeploys.map(_.deploy.sig).toSet
              if (rejectRight && deployIds == rightDeployIds) 0L
              else if (rejectRight && deployIds == leftDeployIds) 100L
              else if (!rejectRight && deployIds == leftDeployIds) 0L
              else if (!rejectRight && deployIds == rightDeployIds) 100L
              else
                throw new Exception(
                  s"something wrong with the tests with reject options " +
                    s"${rejectRight}, ${deployIds}, ${rightDeployIds}, ${leftDeployIds}"
                )
            }
            mergedState <- DagMerger.merge[F](
                            dag,
                            bBlock.blockHash,
                            baseCheckpoint.root,
                            indices(_).deployChains.pure[F],
                            historyRepo,
                            rejectAlg
                          )
            result <- checkFunction(runtime, historyRepo, mergedState)
          } yield result
      }

  }
}
