package coop.rchain.casper.util.rholang
import cats.Parallel
import cats.effect.{ContextShift, Resource, Sync}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.StoreType

import scala.concurrent.ExecutionContext

object Resources {
  def mkRuntimeManager[M[_], F[_]](
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(
      implicit executionContext: ExecutionContext,
      syncF: Sync[M],
      parallelMF: Parallel[M, F],
      contextShift: ContextShift[M]
  ): Resource[M, RuntimeManager[M]] =
    mkRuntime[M, F](prefix)
      .flatMap { runtime =>
        Resource.liftF(RuntimeManager.fromRuntime(runtime))
      }
}
