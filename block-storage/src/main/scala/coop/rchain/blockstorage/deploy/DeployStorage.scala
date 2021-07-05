package coop.rchain.blockstorage.deploy

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed

trait DeployStorage[F[_]] {
  def add(deploys: List[Signed[DeployData]]): F[Unit]
  def remove(deploys: List[Signed[DeployData]]): F[Int]
  def getUnfinalized: F[Set[Signed[DeployData]]]
}

object DeployStorage {
  def apply[F[_]](implicit F: DeployStorage[F]): F.type = F
}
