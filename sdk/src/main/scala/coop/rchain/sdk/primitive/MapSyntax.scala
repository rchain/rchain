package coop.rchain.sdk.primitive

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

trait MapSyntax {
  implicit def mapSyntax[K, V](map: Map[K, V])            = new MapOps[K, V](map)
  implicit def mutableMapSyntax[K, V](map: TrieMap[K, V]) = new MutableMapOps[K, V](map)
}

final case class MapOps[K, V](map: Map[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}

final case class MutableMapOps[K, V](map: mutable.Map[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}
