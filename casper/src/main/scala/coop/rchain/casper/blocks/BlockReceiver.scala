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
// Checking if block should be ignored or continue with processing
case object BeginReceived extends RecvStatus
// Block received and stored in blocks store, waiting for validation and DAG insertion
case object ReceivedAndStored extends RecvStatus
// Block sent to validation
case object PendingValidation extends RecvStatus
// Requested missing dependencies
case object PendingRequest extends RecvStatus

object BlockReceiverState {
  def apply[MId]: BlockReceiverState[MId] = new BlockReceiverState(
    blocksSt = Map(),
    receiveSt = Map(),
    childRelations = Map()
  )
}

final case class BlockReceiverState[MId](
    /**
      * Blocks received and stored in BlockStore with parent relations
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
    * Keep status of block as received process started
    *  - thread safe sync like opening transaction for a block by block hash
    */
  def beginReceived(id: MId): (BlockReceiverState[MId], Boolean) = {
    val expectedReceive = receiveSt.get(id).collect { case PendingRequest => true }.getOrElse(true)
    // If state is not known or pending it's request, continue with receiving
    if (expectedReceive) {
      // Update state to begin received status
      val newReceiveSt = receiveSt + ((id, BeginReceived))
      (copy(receiveSt = newReceiveSt), true)
    } else {
      (this, false)
    }
  }

  /**
    * End of block receiving status update
    *  - like closing transaction, block received and stored
    *
    *  @return unseen parent dependencies
    */
  def endReceived(id: MId, parents: List[(MId, Boolean)]): (BlockReceiverState[MId], Set[MId]) = {
    val curStateOpt = receiveSt.get(id)
    assert(
      curStateOpt == BeginReceived.some,
      s"Received should be called only in begin received state, actual: $curStateOpt, hash: $id"
    )
    curStateOpt
      .collect {
        case BeginReceived =>
          // Update blocks state, keep unseen parents only
          val parentsNotStored = parents.filter(_._2).map(_._1).toSet
          val unseenParents    = parentsNotStored -- blocksSt.keySet -- receiveSt.keySet - id
          val newBlocksSt      = blocksSt + ((id, unseenParents))

          // Update block status to received and set unseen parents to Pending receive state
          val newReceiveStored  = receiveSt + ((id, ReceivedAndStored))
          val newPendingReceive = unseenParents.map((_, PendingRequest))
          val newReceiveSt      = newReceiveStored ++ newPendingReceive

          // Update children relations of received block
          val parentsHashes = parents.map(_._1)
          val newChildRelations = parentsHashes.foldLeft(childRelations) {
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
      case ReceivedAndStored =>
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
      casperShardConf: CasperShardConf
  ): F[Stream[F, BlockHash]] = {

    def blockStr(b: BlockMessage) = PrettyPrinter.buildString(b, short = true)
    def logNotOfInterest(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is not of interest. Dropped")
    def logMalformed(b: BlockMessage) =
      Log[F].info(s"Block ${blockStr(b)} is malformed. Dropped")
//    def logMissingDeps(b: BlockMessage) = Log[F].info(s"Block ${blockStr(b)} missing dependencies.")

    // CHeck if input string is equal to configuration shard ID
    def checkIfEqualToConfigShardId(shardId: String) = {
      val expectedShard = casperShardConf.shardName
      val isValid       = expectedShard == shardId
      def logMsg        = s"Ignored block with invalid shard, expected: $expectedShard, received: $shardId"
      Log[F].info(logMsg).whenA(!isValid).as(isValid)
    }

    // Check if block data is cryptographically safe
    def checkIntegrity(b: BlockMessage): F[Boolean] = Sync[F].defer {
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
          // Filter (ignore) blocks which doesn't pass integrity check
          checkIntegrity(block).flatTap(logMalformed(block).unlessA(_))
        }
        .parEvalMapUnorderedProcBounded { block =>
          // Start block checking, mark status in the state
          val shouldCheck = state.modify(_.beginReceived(block.blockHash))
          // Save block to store and mark end of checking
          val markReceivedAndStore =
            for {
              _ <- BlockStore[F].put(block)
              parents <- block.justifications
                          .map(_.latestBlockHash)
                          .traverse { hash =>
                            BlockStore[F].contains(hash).not.map((hash, _))
                          }
              pendingRequests <- state.modify(_.endReceived(block.blockHash, parents))

              // Send request to missing dependencies
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

    Queue.unbounded[F, BlockHash].map { q =>
      q.dequeue concurrently incomingBlocks(q) concurrently validatedBlocks(q)
    }
  }
}
