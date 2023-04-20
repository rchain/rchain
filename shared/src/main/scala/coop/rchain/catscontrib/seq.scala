package coop.rchain.catscontrib

import cats.{Always, Applicative, Eval, Traverse}

object seq extends SeqInstances

trait SeqInstances {

  implicit val traverseSeq: Traverse[Seq] = new Traverse[Seq] {

    def traverse[G[_], A, B](fa: Seq[A])(f: A => G[B])(implicit G: Applicative[G]): G[Seq[B]] =
      foldRight[A, G[Seq[B]]](fa, Always(G.pure(Seq.empty[B]))) { (a, acc) =>
        G.map2Eval(f(a), acc)(_ +: _)
      }.value

    def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: Seq[A]): Eval[B] =
        as match {
          case Nil    => lb
          case h +: t => f(h, Eval.defer(loop(t)))
        }

      Eval.defer(loop(fa))
    }
  }
}
