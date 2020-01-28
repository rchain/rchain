package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.GUnforgeable
import coop.rchain.models.GUnforgeable.UnfInstance.{
  Empty,
  GDeployIdBody,
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}

private[sorter] object UnforgeableSortMatcher extends Sortable[GUnforgeable] {
  def sortMatch[F[_]: Sync](unf: GUnforgeable): F[ScoredTerm[GUnforgeable]] =
    unf.unfInstance match {
      case GPrivateBody(gpriv) =>
        ScoredTerm(GUnforgeable(GPrivateBody(gpriv)), Node(Score.PRIVATE, Leaf(gpriv.id))).pure[F]
      case GDeployerIdBody(id) =>
        ScoredTerm(
          GUnforgeable(GDeployerIdBody(id)),
          Node(Score.DEPLOYER_AUTH, Leaf(id.publicKey))
        ).pure[F]
      case GDeployIdBody(deployId) =>
        ScoredTerm(GUnforgeable(GDeployIdBody(deployId)), Node(Score.DEPLOY_ID, Leaf(deployId.sig)))
          .pure[F]
      case GSysAuthTokenBody(token) =>
        ScoredTerm(GUnforgeable(GSysAuthTokenBody(token)), Node(Score.SYS_AUTH_TOKEN)).pure[F]
      case Empty => ScoredTerm(unf, Node(Score.ABSENT)).pure[F]
    }
}
