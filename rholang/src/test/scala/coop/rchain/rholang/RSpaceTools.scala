package coop.rchain.rholang
import java.nio.file.Path

import cats.effect.{Resource, Sync}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Context, RSpace}
import coop.rchain.rholang.ResourceTools.mkTempDir

object RSpaceTools {
  def mkRhoISpace[F[_]: Sync, A](
      prefix: String = "",
      branch: String = "test",
      mapSize: Long = 1024L * 1024L * 4
  ): Resource[F, RhoISpace[F]] = {
    import coop.rchain.rholang.interpreter.storage.implicits._

    def mkRspace(dbDir: Path): F[RhoISpace[F]] = {
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

    mkTempDir(prefix)
      .flatMap(tmpDir => Resource.liftF(mkRspace(tmpDir)))
  }
}
