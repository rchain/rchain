package coop.rchain.shared

object MapOps {
  def zip[K, V1, V2](map1: Map[K, V1], map2: Map[K, V2], v1: V1, v2: V2): Map[K, (V1, V2)] =
    (map1.keySet ++ map2.keySet).map { i =>
      (i, (map1.getOrElse(i, v1), map2.getOrElse(i, v2)))
    }.toMap
}
