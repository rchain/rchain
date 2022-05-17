package coop.rchain.rspace.history.syntax
import coop.rchain.rspace.history.KeySegment

trait KeySegmentSyntax {
  implicit def keySegmentSyntax[K, W](v: K): KeySegmentOps[K, W] =
    new KeySegmentOps(v)
}

final class KeySegmentOps[K, W](private val v: K) extends AnyVal {
  def size(implicit ks: KeySegment[K, W]): Int             = ks.size(v)
  def isEmpty(implicit ks: KeySegment[K, W]): Boolean      = ks.isEmpty(v)
  def headOption(implicit ks: KeySegment[K, W]): Option[W] = ks.headOption(v)
  def tailOption(implicit ks: KeySegment[K, W]): Option[K] = ks.tailOption(v)
  def nonEmpty(implicit ks: KeySegment[K, W]): Boolean     = !isEmpty(ks)
  def head(implicit ks: KeySegment[K, W]): W = {
    val hOpt = headOption(ks)
    assert(hOpt.isDefined, "Calling head on empty KeySegment.")
    hOpt.get
  }
  def tail(implicit ks: KeySegment[K, W]): K = {
    val tOpt = tailOption(ks)
    assert(tOpt.isDefined, "Calling tail on empty KeySegment.")
    tOpt.get
  }
  def :+(w: W)(implicit ks: KeySegment[K, W]): K = ks :+ (v, w)
  def ++(k: K)(implicit ks: KeySegment[K, W]): K = ks ++ (v, k)
  def commonPrefix(k: K)(implicit ks: KeySegment[K, W]): (K, K, K) =
    KeySegment.commonPrefix(v, k)
}
