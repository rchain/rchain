package coop.rchain.rholang.interpreter.compiler

import coop.rchain.models.{Expr, Par}

object Visit {

  /**
    * Input data to the normalizer
    *
    * @param par collection of things that might be run in parallel
    * @param env
    * @param knownFree
    */
  final case class ProcVisitInputs(
      par: Par,
      env: IndexMapChain[VarSort],
      knownFree: DeBruijnLevelMap[VarSort]
  )
  // Returns the update Par and an updated map of free variables.
  final case class ProcVisitOutputs(par: Par, knownFree: DeBruijnLevelMap[VarSort])

  final case class NameVisitInputs(
      env: IndexMapChain[VarSort],
      knownFree: DeBruijnLevelMap[VarSort]
  )
  final case class NameVisitOutputs(chan: Par, knownFree: DeBruijnLevelMap[VarSort])

  final case class CollectVisitInputs(
      env: IndexMapChain[VarSort],
      knownFree: DeBruijnLevelMap[VarSort]
  )
  final case class CollectVisitOutputs(expr: Expr, knownFree: DeBruijnLevelMap[VarSort])
}

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort
