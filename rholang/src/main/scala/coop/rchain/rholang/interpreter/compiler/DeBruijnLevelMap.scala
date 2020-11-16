package coop.rchain.rholang.interpreter.compiler

import coop.rchain.models.Connective.ConnectiveInstance

// Parameterized over T, the kind of typing discipline we are enforcing.

// An index map is implemented as a level map that calculates the index on get.
// This way you don't have to re-number the map, you just calculate the index on
// get.
// Parameterized over T, the kind of typing discipline we are enforcing.
final case class DeBruijnLevelMap[T](
    next: Int,
    env: Map[String, LevelContext[T]],
    wildcards: List[SourcePosition],
    logicalConnectives: List[(ConnectiveInstance, SourcePosition)]
) {

  def put(binding: IdContext[T]): DeBruijnLevelMap[T] =
    binding match {
      case (varName, sort, sourcePosition) =>
        DeBruijnLevelMap[T](
          next + 1,
          env + (varName -> LevelContext(next, sort, sourcePosition)),
          wildcards,
          logicalConnectives
        )
    }

  // Returns the new map, and the first value assigned. Given that they're assigned contiguously
  def put(bindings: List[(String, T, SourcePosition)]): DeBruijnLevelMap[T] =
    bindings.foldLeft(this)((map, binding) => map.put(binding))

  // Returns the new map, and a list of the shadowed variables
  def merge(binders: DeBruijnLevelMap[T]): (DeBruijnLevelMap[T], List[(String, SourcePosition)]) = {
    val finalNext        = next + binders.next
    val finalWildcards   = wildcards ++ binders.wildcards
    val finalConnectives = logicalConnectives ++ binders.logicalConnectives
    val adjustNext       = next
    binders.env.foldLeft((this, List[(String, SourcePosition)]())) {
      case (
          (db: DeBruijnLevelMap[T], shadowed: List[(String, SourcePosition)]),
          (k: String, LevelContext(level, varType, sourcePosition))
          ) =>
        val shadowedNew = if (db.env.contains(k)) (k, sourcePosition) :: shadowed else shadowed
        (
          DeBruijnLevelMap(
            finalNext,
            db.env + (k -> LevelContext(level + adjustNext, varType, sourcePosition)),
            finalWildcards,
            finalConnectives
          ),
          shadowedNew
        )
    }
  }

  // Returns the new map
  def addWildcard(sourcePosition: SourcePosition): DeBruijnLevelMap[T] = {
    val newWildcards: List[SourcePosition] = wildcards :+ sourcePosition
    DeBruijnLevelMap(next, env, newWildcards, logicalConnectives)
  }

  def addLogicalConnective[C <: ConnectiveInstance](
      connective: C,
      sourcePosition: SourcePosition
  ): DeBruijnLevelMap[T] = {
    val newConnectives = logicalConnectives :+ ((connective, sourcePosition))
    DeBruijnLevelMap(next, env, wildcards, newConnectives)
  }

  def get(varName: String): Option[LevelContext[T]] = env.get(varName)

  def count: Int = next + wildcards.length + logicalConnectives.length

  def countNoWildcards: Int = next
}

object DeBruijnLevelMap {
  def empty[T]: DeBruijnLevelMap[T] = DeBruijnLevelMap[T](0, Map.empty, List.empty, List.empty)
}
