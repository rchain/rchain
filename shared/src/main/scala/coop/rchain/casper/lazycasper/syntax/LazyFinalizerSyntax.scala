package coop.rchain.casper.lazycasper.syntax
import coop.rchain.casper.lazycasper.{isSupermajority, Fringe, LazyFinalizer}

trait LazyFinalizerSyntax {
  implicit final def lazyFinalizerSyntax[F[_], M, S](
      c: LazyFinalizer[F, M, S]
  ): LazyFinalizerOps[F, M, S] =
    new LazyFinalizerOps[F, M, S](c)
}

final class LazyFinalizerOps[F[_], M, S](val c: LazyFinalizer[F, M, S]) extends AnyVal {
  import c._

  def findNextCompleteFringe(curCompleteFringe: Fringe[M, S]): F[Option[Fringe[M, S]]] =
    findNextFringe(curCompleteFringe, isSupermajority(_, bondsMap))
}
