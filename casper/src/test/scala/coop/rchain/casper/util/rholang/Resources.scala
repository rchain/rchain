package coop.rchain.casper.util.rholang
import cats.Applicative
import cats.effect.Resource
import coop.rchain.rholang.Resources.mkTempDir
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.StoreType
import monix.execution.Scheduler

object Resources {
  def mkRuntimeManager[F[_]: Applicative](
      prefix: String,
      storageSize: Int = 1024 * 1024,
      storeType: StoreType = StoreType.LMDB
  )(implicit scheduler: Scheduler): Resource[F, RuntimeManager] =
    mkTempDir[F](prefix)
      .flatMap(storageDirectory => {
        val activeRuntime = Runtime.create(storageDirectory, storageSize, storeType)
        Resource.pure(RuntimeManager.fromRuntime(activeRuntime))
      })

}
