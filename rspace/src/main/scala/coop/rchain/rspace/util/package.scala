package coop.rchain.rspace

import scala.collection.immutable.Seq

package object util {

  /**
    * Extracts a continuation from a produce result
    */
  def getK[A, K](t: Option[(K, A)]): K =
    t.map(_._1).get

  /** Runs a continuation with the accompanying data
    */
  def runK[T](t: Option[((T) => Unit, T)]): Unit =
    t.foreach { case (k, data) => k(data) }

  /** Runs a list of continuations with the accompanying data
    */
  def runKs[T](t: Seq[Option[((T) => Unit, T)]]): Unit =
    t.foreach { case Some((k, data)) => k(data); case None => () }
}
