package coop.rchain.catscontrib.laws
import cats.Monad
import cats.kernel.laws.IsEq
import coop.rchain.catscontrib.MonadTrans
import cats.laws._

trait MonadTransLaws[MT[_[_], _]] {
  implicit def MT: MonadTrans[MT]

  def identity[G[_], A](a: A)(implicit G: Monad[G], MTM: Monad[MT[G, *]]): IsEq[MT[G, A]] =
    MT.liftM(G.pure(a)) <-> MTM.pure(a)

  def composition[G[_], A, B](
      ga: G[A],
      f: A => G[B]
  )(implicit G: Monad[G], MTM: Monad[MT[G, *]]): IsEq[MT[G, B]] =
    MT.liftM(G.flatMap(ga)(f)) <-> MTM.flatMap(MT.liftM(ga))(a => MT.liftM(f(a)))
}

object MonadTransLaws {
  def apply[MT[_[_], _]](implicit ev: MonadTrans[MT]): MonadTransLaws[MT] =
    new MonadTransLaws[MT] { def MT: MonadTrans[MT] = ev }
}
