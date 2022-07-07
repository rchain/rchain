package coop.rchain.node.mergeablity

import cats.Parallel
import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.helper.TestRhoRuntime.rhoRuntimeEff
import coop.rchain.casper.merging.{BlockHashDagMerger, BlockIndex, DeployChainIndex}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.rholang.RuntimeDeployResult.UserDeployRuntimeResult
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.syntax._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import coop.rchain.models.syntax._
import coop.rchain.sdk.dag.merging.DagMergingLogic

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
      baseDeployRand: Blake2b512Random,
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
                               runtime.processDeployWithMergeableData(_, baseDeployRand)
                             )
            (baseDeploys, baseMergeChs, _) = baseDeploysRes
              .map(UserDeployRuntimeResult.unapply(_).get)
              .unzip3
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${baseDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(baseDeploys.exists(_.isFailed))
            baseCheckpoint <- runtime.createCheckpoint
            leftDeploysRes <- leftDeploySources.toList.traverse(
                               runtime
                                 .processDeployWithMergeableData(_, Blake2b512Random.defaultRandom)
                             )
            (leftDeploys, leftMergeChs, _) = leftDeploysRes
              .map(UserDeployRuntimeResult.unapply(_).get)
              .unzip3
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${leftDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(leftDeploys.exists(_.isFailed))
            leftCheckpoint @ _ <- runtime.createCheckpoint
            _                  <- runtime.reset(baseCheckpoint.root)
            rightDeploysRes <- rightDeploySources.toList.traverse(
                                runtime
                                  .processDeployWithMergeableData(_, Blake2b512Random.defaultRandom)
                              )
            (rightDeploys, rightMergeChs, _) = rightDeploysRes
              .map(UserDeployRuntimeResult.unapply(_).get)
              .unzip3
            _ <- Sync[F]
                  .raiseError(
                    new Exception(s"Process deploy ${rightDeploys.filter(_.isFailed)} failed")
                  )
                  .whenA(rightDeploys.exists(_.isFailed))
            rightCheckpoint @ _ <- runtime.createCheckpoint
            bBlock = getRandomBlock(
              setPreStateHash = RuntimeManager.emptyStateHashFixed.some,
              setPostStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setJustifications = List.empty.some
            )
            rBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(rightCheckpoint.root.bytes.toArray).some,
              setJustifications = List(bBlock.blockHash).some
            )
            lBlock = getRandomBlock(
              setPreStateHash = ByteString.copyFrom(baseCheckpoint.root.bytes.toArray).some,
              setPostStateHash = ByteString.copyFrom(leftCheckpoint.root.bytes.toArray).some,
              setJustifications = List(bBlock.blockHash).some
            )
            leftIndex <- BlockIndex(
                          lBlock.blockHash,
                          leftDeploys,
                          List.empty,
                          baseCheckpoint.root,
                          leftCheckpoint.root,
                          historyRepo,
                          leftMergeChs
                        )
            rightIndex <- BlockIndex(
                           rBlock.blockHash,
                           rightDeploys,
                           List.empty,
                           baseCheckpoint.root,
                           rightCheckpoint.root,
                           historyRepo,
                           rightMergeChs
                         )
            baseIndex <- BlockIndex(
                          bBlock.blockHash,
                          List.empty,
                          List.empty,
                          baseCheckpoint.root, // this does not matter
                          baseCheckpoint.root,
                          historyRepo,
                          Seq.empty
                        )
            kvm      = new InMemoryStoreManager
            dagStore <- BlockDagKeyValueStorage.create[F](kvm)
            _        <- dagStore.insert(bBlock, false, approved = true)
            _        <- dagStore.insert(lBlock, false)
            _        <- dagStore.insert(rBlock, false)
            dag      <- dagStore.getRepresentation
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
            r <- BlockHashDagMerger.merge[F](
                  Set(lBlock, rBlock).map(_.blockHash),
                  Set(bBlock.blockHash),
                  baseCheckpoint.root,
                  BlockHashDagMerger(dag.dagMessageState.msgMap),
                  dag.fringeStates,
                  historyRepo,
                  indices(_: BlockHash).pure,
                  rejectionCost = rejectAlg
                )
            (mergedState, toReject) = r
            result <- checkFunction(
                       runtime,
                       historyRepo,
                       (mergedState, toReject.toSeq)
                     )
          } yield result
      }
  }
}
