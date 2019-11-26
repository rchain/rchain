package coop.rchain.blockstorage.deploy

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed

trait DeployStorage[F[_]] {
  def put(deploys: List[Signed[DeployData]]): F[Unit]
  def remove(deploys: List[Signed[DeployData]]): F[Int]
  def getUnfinalized: F[Set[Signed[DeployData]]]
}

object DeployStorage {
  def apply[F[_]](implicit ev: DeployStorage[F]): DeployStorage[F] = ev
}
