package coop.rchain.sdk.primitive

import scala.collection.concurrent.TrieMap

trait MapSyntax {
  implicit def mapSyntax[K, V](map: Map[K, V])         = new MapOps[K, V](map)
  implicit def trieMapSyntax[K, V](map: TrieMap[K, V]) = new TrieMapOps[K, V](map)
}

final case class MapOps[K, V](map: Map[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}

final case class TrieMapOps[K, V](map: TrieMap[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}
