package coop.rchain.blockstorage.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, RChainState}

object BlockMessageUtil {
  // TODO: Remove once optional fields are removed
  def blockNumber(b: BlockMessage): Long =
    (for {
      bd <- b.body
      ps <- bd.state
    } yield ps.blockNumber).getOrElse(0L)

  def bonds(b: BlockMessage): Seq[Bond] =
    (for {
      bd <- b.body
      ps <- bd.state
    } yield ps.bonds).getOrElse(List.empty[Bond])

  def parentHashes(b: BlockMessage): Seq[ByteString] =
    b.header.fold(Seq.empty[ByteString])(_.parentsHashList)

  def weightMap(blockMessage: BlockMessage): Map[ByteString, Long] =
    blockMessage.body match {
      case Some(block) =>
        block.state match {
          case Some(state) => weightMap(state)
          case None        => Map.empty[ByteString, Long]
        }
      case None => Map.empty[ByteString, Long]
    }

  private def weightMap(state: RChainState): Map[ByteString, Long] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap
}
