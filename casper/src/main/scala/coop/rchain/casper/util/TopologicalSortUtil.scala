package coop.rchain.casper.util

import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil.blockNumber

object TopologicalSortUtil {
  type BlockSort = Vector[Vector[BlockHash]]
  def update(sort: BlockSort, offset: Long, block: BlockMessage): BlockSort = {
    val hash             = block.blockHash
    val offsetDiff: Long = blockNumber(block) - offset

    assert(offsetDiff <= Int.MaxValue)
    val number = offsetDiff.toInt

    //block numbers must be sequential, so a new block can only be
    //at a known height or 1 greater than a known height
    assert(number <= sort.length)

    number match {
      //this is a new block height
      case n if n == sort.length => sort :+ Vector(hash)

      //this is another block at a known height
      case n if n < sort.length =>
        val curr = sort(number)
        sort.updated(number, curr :+ hash)
    }
  }
}
