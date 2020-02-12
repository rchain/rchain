package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed

object SystemDeployUtil {
  // Currently we have 4 system deploys -> refund, preCharge, closeBlock, Slashing
  // In every user deploy, the rnode would do the preCharge first, then execute the
  // user deploy and do the refund at last.
  //
  // The refund and preCharge system deploy
  // would use user deploy signature to generate the system deploy. The random seed of
  // the refund and preCharge has to be exactly the same to make sure replay the user
  // deploy would come out the exact same result.
  //
  // As for closeBlock and slashing, the rnode would execute closeBlock system deploy in every block.
  // The closeBlock system deploy would use the signature of the all user deploys in the block.

  def generateSystemDeployRandomSeed(deploys: Seq[Signed[DeployData]]): Blake2b512Random =
    Tools.rng(
      deploys
        .map(s => s.sig.toByteArray)
        .foldLeft(Array[Byte]())(_ ++ _)
    )

  // splitByte here to make sure random seed is not the same as precharge deploy when there
  // is only one user deploy in a block
  def generateCloseDeployRandomSeed(deploys: Seq[Signed[DeployData]]): Blake2b512Random =
    generateSystemDeployRandomSeed(deploys).splitByte(1)

  def generateSlashDeployRandomSeed(deploys: Seq[Signed[DeployData]]): Blake2b512Random =
    generateSystemDeployRandomSeed(deploys).splitByte(2)

  def generatePreChargeDeployRandomSeed(deploy: Signed[DeployData]): Blake2b512Random =
    Tools.rng(deploy.sig.toByteArray)

  def generateRefundDeployRandomSeed(deploy: Signed[DeployData]): Blake2b512Random =
    Tools.rng(deploy.sig.toByteArray).splitByte(64)

}
