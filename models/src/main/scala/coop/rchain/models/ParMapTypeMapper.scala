package coop.rchain.models

import scalapb.TypeMapper

object ParMapTypeMapper {
  implicit val parMapEMapTypeMapper: TypeMapper[EMap, ParMap] =
    TypeMapper(emapToParMap)(parMapToEMap)

  private[models] def emapToParMap(emap: EMap): ParMap = {
    def unzip(kvp: KeyValuePair): (Par, Par) = (kvp.key, kvp.value)
    ParMap(emap.kvs.map(unzip), emap.connectiveUsed, emap.locallyFree)
  }

  private[models] def parMapToEMap(parMap: ParMap): EMap = {
    def toKeyValuePair(tuple: (Par, Par)): KeyValuePair = KeyValuePair(tuple._1, tuple._2)
    EMap(parMap.ps.sortedMap.map(toKeyValuePair), parMap.locallyFree.value, parMap.connectiveUsed)
  }
}
