package coop.rchain.blockstorage.util

import coop.rchain.casper.protocol.{BlockMessage, Bond, DeployData}
import coop.rchain.crypto.signatures.Signed

object BlockMessageUtil {
  def bonds(b: BlockMessage): Seq[Bond] =
    b.body.state.bonds

  def deployData(b: BlockMessage): Seq[Signed[DeployData]] =
    b.body.deploys.map(_.deploy)
}
