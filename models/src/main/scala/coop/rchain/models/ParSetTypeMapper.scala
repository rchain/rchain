package coop.rchain.models

import scalapb.TypeMapper

object ParSetTypeMapper {
  implicit val parSetESetTypeMapper: TypeMapper[ESet, ParSet] =
    TypeMapper(esetToParSet)(parSetToESet)

  private[models] def esetToParSet(eset: ESet): ParSet =
    ParSet(ps = eset.ps, connectiveUsed = eset.connectiveUsed)

  private[models] def parSetToESet(parSet: ParSet): ESet =
    ESet(parSet.ps, parSet.locallyFree, parSet.connectiveUsed)
}
