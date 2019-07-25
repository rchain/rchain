package coop.rchain.models

import coop.rchain.models.rholang.sorter._
import monix.eval.Coeval

import scala.collection.immutable.{HashMap, TreeSet}

final case class SortedParMap private (
    private[models] val scoredKeySet: TreeSet[ScoredTerm[Par]],
    hashMap: HashMap[Par, Par]
) extends Iterable[(Par, Par)] {

  def +(kv: (Par, Par)): SortedParMap = {
    val scoredKey   = sort(kv._1)
    val scoredValue = sort(kv._2)
    val sortedKey   = scoredKey.term
    val sortedValue = scoredValue.term
    this.copy(
      scoredKeySet = scoredKeySet + scoredKey,
      hashMap = hashMap + (sortedKey -> sortedValue)
    )
  }

  def ++(kvs: (Par, Par)*): SortedParMap = {
    val scoredKeys = kvs.map(kv => sort(kv._1))
    val sortedKvs  = kvs.map { case (key, value) => (sort(key).term, sort(value).term) }
    this.copy(
      scoredKeySet = scoredKeySet ++ scoredKeys,
      hashMap = hashMap ++ sortedKvs
    )
  }

  def -(key: Par): SortedParMap = {
    val scoredKey = sort(key)
    val sortedKey = scoredKey.term
    this.copy(
      scoredKeySet = scoredKeySet - scoredKey,
      hashMap = hashMap - sortedKey
    )
  }

  def --(keys: Par*): SortedParMap = {
    val scoredKeys = keys.map(sort)
    val sortedKeys = scoredKeys.map(_.term)
    this.copy(
      scoredKeySet = scoredKeySet -- scoredKeys,
      hashMap = hashMap -- sortedKeys
    )
  }

  def apply(par: Par): Par = hashMap(sort(par).term)

  def contains(par: Par): Boolean = hashMap.contains(sort(par).term)

  def get(key: Par): Option[Par] = hashMap.get(sort(key).term)

  def getOrElse(key: Par, default: Par): Par = hashMap.getOrElse(sort(key).term, default)

  def keys: Seq[Par] = hashMap.keys.toSeq

  // TODO: Implement values method in Rholang
  def values: Seq[Par] = hashMap.values.toSeq

  def iterator: Iterator[(Par, Par)] = hashMap.iterator

  override def equals(that: Any): Boolean = that match {
    case spm: SortedParMap => spm.hashMap == this.hashMap
    case _                 => false
  }

  override def hashCode(): Int = hashMap.hashCode()

  private def sort(par: Par): ScoredTerm[Par] = Sortable[Par].sortMatch[Coeval](par).value()

}

object SortedParMap {

  def apply(seq: Seq[(Par, Par)]): SortedParMap = {
    val scoredSeq = seq.map {
      case (key, value) =>
        (
          Sortable[Par].sortMatch[Coeval](key).value(),
          Sortable[Par].sortMatch[Coeval](value).value()
        )
    }
    val sortedSeq = scoredSeq.map {
      case (scoredKey, scoredValue) =>
        (scoredKey.term, scoredValue.term)
    }
    new SortedParMap(TreeSet(scoredSeq.map(_._1): _*), HashMap(sortedSeq: _*))
  }

  def empty: SortedParMap = new SortedParMap(TreeSet.empty, HashMap.empty)

}
