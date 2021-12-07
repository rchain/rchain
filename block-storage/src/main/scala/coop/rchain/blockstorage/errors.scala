package coop.rchain.blockstorage

import coop.rchain.casper.protocol.BlockMessage

sealed abstract class StorageError extends Exception

final case class TopoSortFragmentParameterError(startBlockNumber: Long, endBlockNumber: Long)
    extends StorageError
final case class BlockSenderIsMalformed(block: BlockMessage) extends StorageError
