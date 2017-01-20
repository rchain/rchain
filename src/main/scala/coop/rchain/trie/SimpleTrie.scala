package coop.rchain.trie

import collection._
import scala.collection.mutable.{Builder, MapBuilder}
import scala.collection.generic.CanBuildFrom

class SimpleTrie[T] extends mutable.Map[String, T] with mutable.MapLike[String, T, SimpleTrie[T]] {
  
  var suffixes: immutable.Map[Char, SimpleTrie[T]] = Map.empty
  var value: Option[T] = None
  
  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))
  
  def withPrefix(s: String): SimpleTrie[T] = 
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None => suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }
  
  override def update(s: String, elem: T) = withPrefix(s).value = Some(elem)
  
  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))
  
  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
    (for ((chr, m) <- suffixes.iterator; 
          (s, v) <- m.iterator) yield (chr +: s, v))
  
  def += (kv: (String, T)): this.type = { update(kv._1, kv._2); this }
  
  def -= (s: String): this.type  = { remove(s); this }
  
  override def empty = new SimpleTrie[T]
}
  
object SimpleTrie extends {
  def empty[T] = new SimpleTrie[T]
  
  def apply[T](kvs: (String, T)*): SimpleTrie[T] = {
    val m: SimpleTrie[T] = empty
    for (kv <- kvs) m += kv
    m
  }
  
  def newBuilder[T]: Builder[(String, T), SimpleTrie[T]] = 
    new MapBuilder[String, T, SimpleTrie[T]](empty)
  
  implicit def canBuildFrom[T]
    : CanBuildFrom[SimpleTrie[_], (String, T), SimpleTrie[T]] = 
      new CanBuildFrom[SimpleTrie[_], (String, T), SimpleTrie[T]] {
        def apply(from: SimpleTrie[_]) = newBuilder[T]
        def apply() = newBuilder[T]
      }
}