package coop.rchain.models

import scalapb.TypeMapper

object ParMapTypeMapper {
  implicit val parMapEMapTypeMapper: TypeMapper[EMap, ParMap] =
    TypeMapper(emapToParMap)(parMapToEMap)

  private[models] def emapToParMap(emap: EMap): ParMap =
    ParMap(emap.kvs.map(unzip), emap.connectiveUsed, emap.locallyFree, emap.remainder)

  private[models] def parMapToEMap(parMap: ParMap): EMap =
    EMap(
      parMap.ps.sortedMap.map(t => zip(t._1, t._2)).toSeq,
      parMap.locallyFree.value,
      parMap.connectiveUsed,
      parMap.remainder
    )

  private[models] def unzip(kvp: KeyValuePair): (Par, Par) = (kvp.key, kvp.value)

  private[models] def zip(k: Par, v: Par): KeyValuePair = KeyValuePair(k, v)
}
