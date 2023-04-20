package coop.rchain.models

import scalapb.TypeMapper
import cats.Eval

object ParSetTypeMapper {
  implicit val parSetESetTypeMapper: TypeMapper[ESet, ParSet] =
    TypeMapper(esetToParSet)(parSetToESet)

  private[models] def esetToParSet(eset: ESet): ParSet =
    ParSet(
      ps = eset.ps,
      locallyFree = Eval.later(eset.locallyFree.get),
      connectiveUsed = eset.connectiveUsed,
      remainder = eset.remainder
    )

  private[models] def parSetToESet(parSet: ParSet): ESet =
    ESet(parSet.ps.sortedPars, parSet.locallyFree.value, parSet.connectiveUsed, parSet.remainder)
}
