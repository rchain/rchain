package coop.rchain.casper.util.rholang

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockMetadata, BlockStore}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.{DagOperations, ProtoUtil}
import coop.rchain.casper.{BlockDag, BlockException, PrettyPrinter}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rspace.ReplayException
import coop.rchain.shared.{Log, LogSource}

import scala.language.higherKinds

object InterpreterUtil {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def mkTerm(rho: String): Either[Throwable, Par] =
    Interpreter.buildNormalizedTerm(rho).runAttempt

  //Returns (None, checkpoints) if the block's tuplespace hash
  //does not match the computed hash based on the deploys
  def validateBlockCheckpoint[F[_]: Monad: Log: BlockStore](
      b: BlockMessage,
      dag: BlockDag,
      runtimeManager: RuntimeManager[F]
  ): F[Either[BlockException, Option[StateHash]]] = {
    val tsHash          = ProtoUtil.tuplespace(b)
    val deploys         = ProtoUtil.deploys(b)
    val internalDeploys = deploys.flatMap(ProcessedDeployUtil.toInternal)
    val timestamp       = Some(b.header.get.timestamp) // TODO: Ensure header exists through type
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)
      possiblePreStateHash <- computeParentsPostState[F](
                               parents,
                               dag,
                               runtimeManager,
                               timestamp
                             )
      result <- processPossiblePreStateHash[F](
                 runtimeManager,
                 tsHash,
                 internalDeploys,
                 possiblePreStateHash,
                 timestamp
               )
    } yield result
  }

  private def processPossiblePreStateHash[F[_]: Monad: Log: BlockStore](
      runtimeManager: RuntimeManager[F],
      tsHash: Option[StateHash],
      internalDeploys: Seq[InternalProcessedDeploy],
      possiblePreStateHash: Either[Throwable, StateHash],
      time: Option[Long]
  ): F[Either[BlockException, Option[StateHash]]] =
    possiblePreStateHash match {
      case Left(ex) =>
        Left(BlockException(ex)).rightCast[Option[StateHash]].pure[F]
      case Right(parentStateHash) =>
        for {
          state <- runtimeManager.replayComputeState(parentStateHash, internalDeploys, time)
          result <- state match {
                     case Left((Some(deploy), status)) =>
                       status match {
                         case InternalErrors(exs) =>
                           Left(
                             BlockException(
                               new Exception(
                                 s"Internal errors encountered while processing ${PrettyPrinter
                                   .buildString(deploy)}: ${exs.mkString("\n")}"
                               )
                             )
                           ).rightCast[Option[StateHash]].pure[F]
                         case UserErrors(errors: Vector[Throwable]) =>
                           Log[F].warn(
                             s"Found user error(s) ${errors.map(_.getMessage).mkString("\n")}"
                           ) *> Right(
                             none[StateHash]
                           ).leftCast[BlockException].pure[F]
                         case ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) =>
                           Log[F].warn(
                             s"Found replay status mismatch; replay failure is ${replay.isFailed} and orig failure is ${orig.isFailed}"
                           ) *> Right(none[StateHash]).leftCast[BlockException].pure[F]
                         case UnknownFailure =>
                           Log[F].warn(s"Found unknown failure") *> Right(none[StateHash])
                             .leftCast[BlockException]
                             .pure[F]
                       }
                     case Left((None, status)) =>
                       status match {
                         case UnusedCommEvent(ex: ReplayException) =>
                           Log[F].warn(s"Found unused comm event ${ex.getMessage}") *> Right(
                             none[StateHash]
                           ).leftCast[BlockException]
                             .pure[F]
                       }
                     case Right(computedStateHash) =>
                       if (tsHash.contains(computedStateHash)) {
                         // state hash in block matches computed hash!
                         Right(Option(computedStateHash))
                           .leftCast[BlockException]
                           .pure[F]
                       } else {
                         // state hash in block does not match computed hash -- invalid!
                         // return no state hash, do not update the state hash set
                         Log[F].warn(
                           s"Tuplespace hash ${tsHash.getOrElse(ByteString.EMPTY)} does not match computed hash $computedStateHash."
                         ) *> Right(none[StateHash]).leftCast[BlockException].pure[F]
                       }
                   }
        } yield result
    }
  def computeDeploysCheckpoint[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      deploys: Seq[Deploy],
      dag: BlockDag,
      runtimeManager: RuntimeManager[F],
      time: Option[Long] = None
  ): F[Either[Throwable, (StateHash, Seq[InternalProcessedDeploy])]] =
    for {
      possiblePreStateHash <- computeParentsPostState[F](parents, dag, runtimeManager, time)
      result <- possiblePreStateHash match {
                 case Right(preStateHash) =>
                   runtimeManager.computeState(preStateHash, deploys, time).map(Right(_))
                 case Left(err) =>
                   Monad[F].pure(Left(err))
               }
    } yield result

  private def computeParentsPostState[F[_]: Monad: BlockStore](
      parents: Seq[BlockMessage],
      dag: BlockDag,
      runtimeManager: RuntimeManager[F],
      time: Option[Long]
  ): F[Either[Throwable, StateHash]] = {
    val parentTuplespaces = parents.flatMap(p => ProtoUtil.tuplespace(p).map(p -> _))

    parentTuplespaces match {
      //no parents to base off of, so use default
      case Seq() =>
        Right(runtimeManager.emptyStateHash).leftCast[Throwable].pure[F]

      //For a single parent we look up its checkpoint
      case Seq((_, parentStateHash)) =>
        Right(parentStateHash).leftCast[Throwable].pure[F]

      //In the case of multiple parents we need
      //to apply all of the deploys that have been
      //made in all of the branches of the DAG being
      //merged. This is done by computing uncommon ancestors
      //and applying the deploys in those blocks.
      case (initParent, initStateHash) +: _ =>
        implicit val ordering: Ordering[BlockMetadata] = BlockDag.deriveOrdering(dag)
        val indexedParents                             = parents.toVector.map(b => dag.dataLookup(b.blockHash))
        val uncommonAncestors                          = DagOperations.uncommonAncestors(indexedParents, dag.dataLookup)

        val initIndex = indexedParents.indexOf(dag.dataLookup(initParent.blockHash))
        //filter out blocks that already included by starting from the chosen initParent
        val blocksToApply = uncommonAncestors
          .filterNot { case (_, set) => set.contains(initIndex) }
          .keys
          .toVector
          .sorted //ensure blocks to apply is topologically sorted to maintain any causal dependencies

        for {
          maybeBlocks <- blocksToApply.traverse[F, Option[BlockMessage]](
                          b => BlockStore[F].get(b.blockHash)
                        )
          _       = assert(maybeBlocks.forall(_.isDefined))
          blocks  = maybeBlocks.flatten
          deploys = blocks.flatMap(_.getBody.deploys.flatMap(ProcessedDeployUtil.toInternal))
          state   <- runtimeManager.replayComputeState(initStateHash, deploys, time)
        } yield
          state match {
            case result @ Right(_) => result.leftCast[Throwable]
            case Left((_, status)) =>
              val parentHashes =
                parents.map(p => Base16.encode(p.blockHash.toByteArray).take(8))
              Left(
                new Exception(
                  s"Failed status while computing post state of $parentHashes: $status"
                )
              )
          }
    }
  }

  private[casper] def computeBlockCheckpointFromDeploys[F[_]: Monad: BlockStore](
      b: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag,
      runtimeManager: RuntimeManager[F]
  ): F[Either[Throwable, (StateHash, Seq[InternalProcessedDeploy])]] =
    for {
      parents <- ProtoUtil.unsafeGetParents[F](b)

      deploys = ProtoUtil.deploys(b).flatMap(_.deploy)

      _ = assert(
        parents.nonEmpty || (parents.isEmpty && b == genesis),
        "Received a different genesis block."
      )

      result <- computeDeploysCheckpoint[F](
                 parents,
                 deploys,
                 dag,
                 runtimeManager
               )
    } yield result
}
