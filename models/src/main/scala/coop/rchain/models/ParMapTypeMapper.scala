package coop.rchain.models

import scalapb.TypeMapper

object ParMapTypeMapper {
  implicit val parMapEMapTypeMapper: TypeMapper[EMap, ParMap] =
    TypeMapper(emapToParMap)(parMapToEMap)

  private[models] def emapToParMap(emap: EMap): ParMap =
    ParMap(emap.kvs.map(unzip), emap.connectiveUsed, emap.locallyFree)

  private[models] def parMapToEMap(parMap: ParMap): EMap =
    EMap(parMap.ps.sortedMap.map(t => zip(t._1, t._2)),
         parMap.locallyFree.value,
         parMap.connectiveUsed)

  private[models] def unzip(kvp: KeyValuePair): (Par, Par) = (kvp.key, kvp.value)

  private[models] def zip(k: Par, v: Par): KeyValuePair = KeyValuePair(k, v)
}
