package coop.rchain.rspace.state.instances

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.rspace.state.instances.RSpaceExporterImpl.NoRootError
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter, RSpaceStateManager}

object RSpaceStateManagerImpl {
  def apply[F[_]: Sync](
      exporter: RSpaceExporter[F],
      importer: RSpaceImporter[F]
  ): RSpaceStateManager[F] =
    RSpaceStateManagerImpl[F](exporter, importer)

  private final case class RSpaceStateManagerImpl[F[_]: Sync](
      exporter: RSpaceExporter[F],
      importer: RSpaceImporter[F]
  ) extends RSpaceStateManager[F] {

    override def isEmpty: F[Boolean] = hasRoot

    def hasRoot: F[Boolean] =
      exporter.getRoot.map(kp(true)).handleError {
        case NoRootError => false
      }
  }
}
