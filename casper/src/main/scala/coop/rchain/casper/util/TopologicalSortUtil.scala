package coop.rchain.casper.util

import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.blockNumber

object TopologicalSortUtil {
  type BlockSort = Vector[Vector[BlockHash]]
  def update(sort: BlockSort, offset: Long, block: BlockMessage): BlockSort = {
    val hash   = block.blockHash
    val number = (blockNumber(block) - offset).toInt

    if (number == sort.length) {
      //this is a new block height
      sort :+ Vector(hash)
    } else if (number < sort.length) {
      //this is another block at a known height
      val curr = sort(number)
      sort.updated(number, curr :+ hash)
    } else {
      //impossible because block numbers must be sequential
      throw new Exception("Attempted to add block with invalid block number to state.")
    }
  }

}
