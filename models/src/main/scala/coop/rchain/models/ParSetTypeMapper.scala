package coop.rchain.models

import scalapb.TypeMapper
import monix.eval.Coeval

object ParSetTypeMapper {
  implicit val parSetESetTypeMapper: TypeMapper[ESet, ParSet] =
    TypeMapper(esetToParSet)(parSetToESet)

  private[models] def esetToParSet(eset: ESet): ParSet =
    ParSet(ps = eset.ps,
           locallyFree = Coeval.delay(eset.locallyFree),
           connectiveUsed = eset.connectiveUsed)

  private[models] def parSetToESet(parSet: ParSet): ESet =
    ESet(parSet.ps.sortedPars, parSet.locallyFree.value, parSet.connectiveUsed)
}
