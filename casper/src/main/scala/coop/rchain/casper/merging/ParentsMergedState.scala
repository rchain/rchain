package coop.rchain.casper.merging

import com.google.protobuf.ByteString
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rspace.hashing.Blake2b256Hash

/**
  * [[ParentsMergedState]] represents required data to create or validate a block from the view of
  * parent relations (justifications) or for the new block, latest messages which will be used as justifications.
  *
  * @param justifications block justifications (latest blocks for the new block)
  * @param maxBlockNum maximum block height from parent blocks
  * @param maxSeqNums latest sequence numbers for bonded validators
  * @param fringe finalized fringe seen (finalized) by parents
  * @param fringeState finalized fringe (merged) state
  * @param fringeBondsMap bonds map of validators on finalized fringe state
  * @param fringeRejectedDeploys rejected deploys from blocks finalized with [[fringe]] blocks
  * @param preStateHash state hash after non-finalized blocks are merged
  * @param rejectedDeploys rejected deploys after non-finalized blocks are merged
  */
final case class ParentsMergedState(
    justifications: Set[BlockMetadata],
    maxBlockNum: Long,
    maxSeqNums: Map[Validator, Long],
    // Fringe merged state
    fringe: Set[BlockHash],
    fringeState: Blake2b256Hash,
    fringeBondsMap: Map[Validator, Long],
    fringeRejectedDeploys: Set[ByteString],
    // Conflict scope state (non-finalized blocks)
    preStateHash: Blake2b256Hash,
    rejectedDeploys: Set[ByteString]
)
