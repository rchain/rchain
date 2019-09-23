package coop.rchain.blockstorage.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, DeployData}

object BlockMessageUtil {
  // TODO: Remove once optional fields are removed
  def blockNumber(b: BlockMessage): Long =
    b.body.state.blockNumber

  def bonds(b: BlockMessage): Seq[Bond] =
    b.body.state.bonds

  def deployData(b: BlockMessage): Seq[DeployData] =
    b.body.deploys.map(_.deploy)

  def parentHashes(b: BlockMessage): Seq[ByteString] =
    b.header.parentsHashList
}
