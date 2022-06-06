package coop.rchain.casper.blocks

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{CasperShardConf, PrettyPrinter, Validate}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream
import fs2.concurrent.Queue

sealed trait RecvStatus
// Begin checking and storing block
case object BeginStoreBlock extends RecvStatus
// Block stored in the block store, waiting for validation and DAG insertion
case object EndStoreBlock extends RecvStatus
// Block sent to validation
case object PendingValidation extends RecvStatus
// Requested missing dependencies
case object Requested extends RecvStatus

object BlockReceiverState {
  def apply[MId]: BlockReceiverState[MId] = BlockReceiverState(
    blocksSt = Map(),
    receiveSt = Map(),
    childRelations = Map()
  )
}

/**
  * Block receiver state
  *
  * It consist of three events. Two to store blocks (begin and end) to prevent race when
  * storing blocks and finished when block is validated and added to the DAG (end of processing).
  */
final case class BlockReceiverState[MId](
    /**
      * Blocks received and stored in BlockStore (not validated) with parent relations
      */
    blocksSt: Map[MId, Set[MId]],
    /**
      * Blocks receiving status
      */
    receiveSt: Map[MId, RecvStatus],
    /**
      * Blocks mapping with children relations
      */
    childRelations: Map[MId, Set[MId]]
) {

  /**
    * Begin storing block, mark block to prevent duplicate threads store the same block
    *  - thread safe sync like opening transaction for a block by block hash
    */
  def beginStored(id: MId): (BlockReceiverState[MId], Boolean) = {
    // If state is not known or pending request, it's expected so continue with receiving
    val expectedReceive = receiveSt.get(id).collect { case Requested => true }.getOrElse(true)
    if (expectedReceive) {
      // Update state to begin received status
      val newReceiveSt = receiveSt + ((id, BeginStoreBlock))
      (copy(receiveSt = newReceiveSt), true)
    } else {
      (this, false)
    }
  }

  /**
    * Storing of block done, waiting validation
    *  - like closing transaction, block stored and waiting validation
    *
    *  @return unseen parent dependencies
    */
  def endStored(id: MId, parents: List[(MId, Boolean)]): (BlockReceiverState[MId], Set[MId]) = {
    val curStateOpt = receiveSt.get(id)
    assert(
      curStateOpt == BeginStoreBlock.some,
      s"Received should be called only in begin received state, actual: $curStateOpt, hash: $id"
    )
    curStateOpt
      .collect {
        case BeginStoreBlock =>
          // Update blocks state, keep unseen parents only
          val parentsNotStored = parents.filter(_._2).map(_._1).toSet
          val unseenParents    = parentsNotStored -- blocksSt.keySet -- receiveSt.keySet - id
          val newBlocksSt      = blocksSt + ((id, unseenParents))

          // Update block status to received and set unseen parents to Pending receive state
          val newReceiveStored  = receiveSt + ((id, EndStoreBlock))
          val newPendingReceive = unseenParents.map((_, Requested))
          val newReceiveSt      = newReceiveStored ++ newPendingReceive

          // Update children relations of received block
          val parentIds = parents.map(_._1)
          val newChildRelations = parentIds.foldLeft(childRelations) {
            case (acc, parent) =>
              val childs = acc.getOrElse(parent, Set())
              acc + ((parent, childs + id))
          }

          // New state
          val newState = copy(
            blocksSt = newBlocksSt,
            receiveSt = newReceiveSt,
            childRelations = newChildRelations
          )

          (newState, unseenParents)
      }
      .getOrElse((this, Set()))
  }

  /**
    * Finished block validation, update state and return next blocks
    *
    * @return next blocks with validated dependencies
    */
  def finished(id: MId, parents: Set[MId]): (BlockReceiverState[MId], Set[MId]) = {
    val parentsInState = blocksSt.get(id)
    val isReceived = receiveSt.get(id).collect {
      case EndStoreBlock     =>
      case PendingValidation =>
    }
    (parentsInState <* isReceived)
      .as {
        // Update blocks state
        //  - remove finished block from child dependencies and remove finished block
        //  - remove finished block from blocks state
        val childs        = childRelations.get(id).toList.flatten
        val updatedBlocks = childs.map(b => (b, blocksSt(b) - id)).toMap
        val newBlocksSt   = blocksSt ++ updatedBlocks - id

        // Get next blocks with all dependencies validated and not already in pending validation state
        val depsValidated = updatedBlocks.filter {
          case (bid, parents) =>
            def pending = receiveSt.get(bid).contains(PendingValidation)
            parents.isEmpty && !pending
        }.keySet

        // Update received state
        //  - set to pending validation state
        //  - remove finished block from received state
        val depsValidatedPending = depsValidated.map((_, PendingValidation))
        val newReceiveSt         = receiveSt ++ depsValidatedPending - id

        // Remove finished block from children relations
        val newChildRelations = parents.foldLeft(childRelations) {
          case (acc, parent) =>
            val childs = acc.getOrElse(parent, Set()) - id
            if (childs.isEmpty) acc - parent
            else acc + ((parent, childs))
        }

        // New state
        val newState = copy(
          blocksSt = newBlocksSt,
          receiveSt = newReceiveSt,
          childRelations = newChildRelations
        )

        (newState, depsValidated)
      }
      .getOrElse((this, Set()))
  }
}

object BlockReceiver {
  def apply[F[_]: Concurrent: BlockStore: BlockDagStorage: BlockRetriever: Log](
      state: Ref[F, BlockReceiverState[BlockHash]],
      incomingBlocksStream: Stream[F, BlockMessage],
      finishedProcessingStream: Stream[F, BlockMessage],
      confShardName: String
  ): F[Stream[F, BlockHash]] = {

    def blockStr(b: BlockMessage) = PrettyPrinter.buildString(b, short = true)
    def logNotOfInterest(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is not of interest. Dropped")
    def logMalformed(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is malformed. Dropped")

    // TODO: add logging of missing dependencies
    // def logMissingDeps(b: BlockMessage) = Log[F].info(s"Block ${blockStr(b)} missing dependencies.")

    // Check if input string is equal to configuration shard ID
    def checkIfEqualToConfigShardId(shardId: String) = {
      val isValid = confShardName == shardId
      def logMsg  = s"Ignored block with invalid shard, expected: $confShardName, received: $shardId"
      Log[F].info(logMsg).whenA(!isValid).as(isValid)
    }

    // Check if block data is cryptographically safe and part of the same shard
    def checkIfOfInterest(b: BlockMessage): F[Boolean] = Sync[F].defer {
      val validShard = checkIfEqualToConfigShardId(b.shardId)
      // TODO: 1. validation and logging in these checks should be separated
      //       2. logging of these block should indicate that information cannot be trusted
      //           e.g. if block hash is invalid it cannot represent identity of a block
      val validFormat = Validate.formatOfFields(b)
      val validHash   = Validate.blockHash(b)
      val validSig    = Validate.blockSignature(b)
      // TODO: check sender to be valid bonded validator
      //  - not always possible because now are new blocks downloaded from DAG tips
      //    which in case of epoch change sender can be unknown
      // TODO: check valid version (possibly part of hash checking)
      validFormat &&^ validShard &&^ validHash &&^ validSig
    }

    // Check if block should be stored
    def checkIfKnown(b: BlockMessage): F[Boolean] = {
      val isStored = BlockStore[F].contains(b.blockHash)
      val isTooOld = BlockDagStorage[F].getRepresentation.map { dag =>
        dag.heightMap.headOption.map(_._1).forall(ProtoUtil.blockNumber(b) < _)
      }
      isStored ||^ isTooOld
    }

    def requestMissingDependencies(deps: Set[BlockHash]): F[Unit] =
      deps.toList.traverse_(
        BlockRetriever[F].admitHash(_, admitHashReason = BlockRetriever.MissingDependencyRequested)
      )

    // Process incoming blocks
    def incomingBlocks(receiverOutputQueue: Queue[F, BlockHash]) =
      incomingBlocksStream
        .evalFilterAsyncUnorderedProcBounded { block =>
          // Filter (ignore) blocks that are not of interest (pass integrity check, incorrect shard or version, ...)
          checkIfOfInterest(block).flatTap(logMalformed(block).unlessA(_))
        }
        .parEvalMapUnorderedProcBounded { block =>
          // Start block checking, mark begin of checking in the state (begin received "transaction")
          val shouldCheck = state.modify(_.beginStored(block.blockHash))
          // Save block to store, mark end of checking in the state (end received "transaction")
          val markReceivedAndStore =
            for {
              // Save block to block store, resolve parents to request
              _ <- BlockStore[F].put(block)
              parents <- block.justifications
                          .map(_.latestBlockHash)
                          .traverse { hash =>
                            BlockStore[F].contains(hash).not.map((hash, _))
                          }
              pendingRequests <- state.modify(_.endStored(block.blockHash, parents))

              // Send request for missing dependencies
              _ <- requestMissingDependencies(pendingRequests)

              // Check if block have all dependencies in the DAG
              dag <- BlockDagStorage[F].getRepresentation
              hasAllDeps = pendingRequests.isEmpty &&
                block.justifications.map(_.latestBlockHash).forall(dag.contains)

              _ <- receiverOutputQueue.enqueue1(block.blockHash).whenA(hasAllDeps)
            } yield ()
          for {
            isOfInterest <- shouldCheck &&^ checkIfKnown(block).not
            // Log if block is ignored
            _ <- logNotOfInterest(block).unlessA(isOfInterest)
            // Save to store if block is of interest and send request for missing dependencies
            _ <- markReceivedAndStore.whenA(isOfInterest)
          } yield ()
        }

    // Process validated blocks
    def validatedBlocks(receiverOutputQueue: Queue[F, BlockHash]) =
      finishedProcessingStream.parEvalMapUnorderedProcBounded { block =>
        val parents = block.justifications.map(_.latestBlockHash).toSet
        for {
          // Update state with finalized block and get next for validation
          next <- state.modify(_.finished(block.blockHash, parents))

          // Notify BlockRetriever of finished validation of block
          _ <- BlockRetriever[F].ackInCasper(block.blockHash)

          // Send dependency free blocks to validation
          _ <- next.toList.traverse_(receiverOutputQueue.enqueue1)
        } yield ()
      }

    // Return output stream, in parallel process incoming and validated blocks
    Queue.unbounded[F, BlockHash].map { outQueue =>
      outQueue.dequeue concurrently incomingBlocks(outQueue) concurrently validatedBlocks(outQueue)
    }
  }
}
