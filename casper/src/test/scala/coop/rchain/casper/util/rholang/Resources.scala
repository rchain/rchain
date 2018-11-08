package coop.rchain.casper.util.rholang
import cats.Applicative
import cats.effect.Resource
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.shared.StoreType
import monix.execution.Scheduler

object Resources {
  def mkRuntimeManager[F[_]: Applicative](
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(implicit scheduler: Scheduler): Resource[F, RuntimeManager] =
    mkRuntime[F](prefix)
      .flatMap { runtime => Resource.pure(RuntimeManager.fromRuntime(runtime))}
}
