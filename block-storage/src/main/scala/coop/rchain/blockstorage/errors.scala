package coop.rchain.blockstorage

sealed abstract class StorageError extends Exception

final case class TopoSortFragmentParameterError(startBlockNumber: Long, endBlockNumber: Long)
    extends StorageError
