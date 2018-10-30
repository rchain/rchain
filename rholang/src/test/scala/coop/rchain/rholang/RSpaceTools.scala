package coop.rchain.rholang
import java.nio.file.Path

import cats.implicits._
import cats.effect.Sync
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Context, RSpace}
import coop.rchain.rholang.ResourceTools.withTempDir

object RSpaceTools {
  def runInRhoISpace[F[_]: Sync, A](
      job: RhoISpace[F] => F[A],
      prefix: String = "",
      branch: String = "test",
      mapSize: Long = 1024L * 1024L * 4
  ): F[A] = {
    import coop.rchain.rholang.interpreter.storage.implicits._

    def mkSpace(dbDir: Path): F[RhoISpace[F]] = {
      val context: RhoContext = Context.create(dbDir, mapSize)

      RSpace.create[
        F,
        Par,
        BindPattern,
        OutOfPhlogistonsError.type,
        ListParWithRandom,
        ListParWithRandomAndPhlos,
        TaggedContinuation
      ](context, Branch(branch))
    }

    withTempDir(prefix)(mkSpace(_).flatMap(job))
  }
}
