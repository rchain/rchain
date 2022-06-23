package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rspace.hashing.Blake2b256Hash

/**
  * [[ParentsPreState]] represents required data to create or validate a block from the view of
  * parent relations (justifications) or for the new block, latest messages which will be used as justifications.
  *
  * @param justifications block justifications (latest blocks for the new block)
  * @param fringe finalized fringe seen (finalized) by parents
  * @param fringeState finalized fringe (merged) state
  * @param bondsMap bonds map of validators on fringe state
  * @param rejectedDeploys rejected deploys from blocks finalized with [[fringe]] blocks
  * @param maxBlockNum maximum block height from parent blocks
  * @param maxSeqNums latest sequence numbers for bonded validators
  */
final case class ParentsPreState(
    justifications: Set[BlockMetadata],
    fringe: Set[BlockHash],
    fringeState: Blake2b256Hash,
    bondsMap: Map[Validator, Long],
    rejectedDeploys: Set[ByteString],
    maxBlockNum: Long,
    maxSeqNums: Map[Validator, Long]
)
