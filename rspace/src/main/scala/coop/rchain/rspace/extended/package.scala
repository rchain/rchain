package coop.rchain.rspace

import scala.annotation.tailrec

package object extended {

  def getK[A, K](t: Option[(K, A)]): K =
    t.map(_._1).get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](t: Option[(Function1[T, Unit], T)]): Unit =
    t.foreach { case (k, data) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: List[Option[(Function1[T, Unit], T)]]): Unit =
    t.foreach { case Some((k, data)) => k(data); case None => () }

  /** Consume all
    */
  def consumeAll[C, P, A, K](
      store: IStore[C, P, A, K],
      channels: List[C],
      patterns: List[P],
      continuation: K)(implicit m: Match[P, A]): List[Option[(K, List[A])]] = {
    @tailrec
    def loop(acc: List[Option[(K, List[A])]]): List[Option[(K, List[A])]] =
      consume(store, channels, patterns, continuation, persist = false) match {
        case r @ Some(_) => loop(r :: acc)
        case None        => acc
      }
    loop(List.empty[Option[(K, List[A])]])
  }

  /** Persistent consume
    */
  def pconsume[C, P, A, K](store: IStore[C, P, A, K],
                           channels: List[C],
                           patterns: List[P],
                           continuation: K)(implicit m: Match[P, A]): List[(K, List[A])] = {
    @tailrec
    def loop(acc: List[(K, List[A])]): List[(K, List[A])] =
      consume(store, channels, patterns, continuation, persist = true) match {
        case Some(res) => loop(res :: acc)
        case None      => acc
      }
    loop(List.empty[(K, List[A])])
  }

  /** Persistent produce
    */
  def pproduce[C, P, A, K](store: IStore[C, P, A, K], channel: C, data: A)(
      implicit m: Match[P, A]): List[(K, List[A])] = {
    @tailrec
    def loop(acc: List[(K, List[A])]): List[(K, List[A])] =
      produce(store, channel, data, persist = true) match {
        case Some(res) => loop(res :: acc)
        case None      => acc
      }
    loop(List.empty[(K, List[A])])
  }
}
