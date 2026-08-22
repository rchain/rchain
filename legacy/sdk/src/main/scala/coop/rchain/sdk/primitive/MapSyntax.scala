package coop.rchain.sdk.primitive

import scala.collection.mutable

trait MapSyntax {
  implicit final def mapSyntax[K, V](value: Map[K, V]) = new MapOps[K, V](value)
  implicit final def mutableMapSyntax[K, V](value: mutable.Map[K, V]) =
    new MutableMapOps[K, V](value)
}

final class MapOps[K, V](private val map: Map[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}

final class MutableMapOps[K, V](private val map: mutable.Map[K, V]) extends AnyVal {
  def getUnsafe(k: K): V = {
    val vOpt = map.get(k)
    require(vOpt.isDefined, s"No key $k in a map.")
    vOpt.get
  }
}
