package coop.rchain.blockstorage.deploy

import cats.Monad
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.applicative._
import cats.syntax.functor._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed

class InMemDeployStorage[F[_]: Monad](
    deployRef: Ref[F, Set[Signed[DeployData]]]
) extends DeployStorage[F] {
  def add(deploys: List[Signed[DeployData]]): F[Unit] = deployRef.update(_ ++ deploys)

  def remove(deploys: List[Signed[DeployData]]): F[Int] =
    deployRef.modify { oldDeploys =>
      val newDeploys = oldDeploys -- deploys
      (newDeploys, oldDeploys.size - newDeploys.size)
    }

  def getUnfinalized: F[Set[Signed[DeployData]]] = deployRef.get
}

object InMemDeployStorage {
  def make[F[_]: Concurrent]: F[DeployStorage[F]] =
    for {
      ref <- Ref.of[F, Set[Signed[DeployData]]](Set.empty)
    } yield new InMemDeployStorage[F](ref)
}
