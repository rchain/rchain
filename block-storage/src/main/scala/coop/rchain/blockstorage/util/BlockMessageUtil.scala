package coop.rchain.blockstorage.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, DeployData}

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

  def deployData(b: BlockMessage): Seq[DeployData] =
    (for {
      bs <- b.body
    } yield bs.deploys.flatMap(_.deploy)).getOrElse(List.empty)

  def parentHashes(b: BlockMessage): Seq[ByteString] =
    b.header.fold(Seq.empty[ByteString])(_.parentsHashList)
}
