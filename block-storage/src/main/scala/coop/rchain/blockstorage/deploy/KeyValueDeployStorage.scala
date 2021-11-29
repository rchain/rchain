package coop.rchain.blockstorage.deploy

import cats.Functor
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs.{codecDeploySignature, codecSignedDeployData}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}

final case class KeyValueDeployStorage[F[_]: Functor] private (
    store: KeyValueTypedStore[F, ByteString, Signed[DeployData]]
) extends DeployStorage[F] {
  def add(deploys: List[Signed[DeployData]]): F[Unit] =
    store.put(deploys.map(d => (d.sig, d)))

  def remove(deploys: List[Signed[DeployData]]): F[Int] =
    store.delete(deploys.map(_.sig))

  def readAll: F[Set[Signed[DeployData]]] =
    store.toMap.map(_.values.toSet)
}

object KeyValueDeployStorage {

  def apply[F[_]: Sync](kvm: KeyValueStoreManager[F]): F[KeyValueDeployStorage[F]] =
    for {
      store <- kvm.database("deploy_storage", codecDeploySignature, codecSignedDeployData)
    } yield KeyValueDeployStorage[F](store)

}
