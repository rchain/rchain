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
  def put(bindings: List[IdContext[T]]): DeBruijnLevelMap[T] =
    bindings.foldLeft(this)((map, binding) => map.put(binding))

  // Returns the new map, and a list of the shadowed variables
  def merge(binders: DeBruijnLevelMap[T]): (DeBruijnLevelMap[T], List[(String, SourcePosition)]) = {

    val (accEnv, shadowed) = binders.env.foldLeft((env, List.empty[(String, SourcePosition)])) {
      case ((accEnv, shadowed), (name, LevelContext(level, typ, sourcePosition))) =>
        (
          accEnv + (name -> LevelContext(level + next, typ, sourcePosition)),
          if (env.contains(name)) (name, sourcePosition) :: shadowed
          else shadowed
        )
    }

    (
      DeBruijnLevelMap(
        next + binders.next,
        accEnv,
        wildcards ++ binders.wildcards,
        logicalConnectives ++ binders.logicalConnectives
      ),
      shadowed
    )
  }

  // Returns the new map
  def addWildcard(sourcePosition: SourcePosition): DeBruijnLevelMap[T] =
    DeBruijnLevelMap(next, env, wildcards :+ sourcePosition, logicalConnectives)

  def addLogicalConnective(
      connective: ConnectiveInstance,
      sourcePosition: SourcePosition
  ): DeBruijnLevelMap[T] =
    DeBruijnLevelMap(next, env, wildcards, logicalConnectives :+ ((connective, sourcePosition)))

  def get(varName: String): Option[LevelContext[T]] = env.get(varName)

  def count: Int = next + wildcards.length + logicalConnectives.length

  def countNoWildcards: Int = next

}

object DeBruijnLevelMap {
  def empty[T]: DeBruijnLevelMap[T] = DeBruijnLevelMap[T](0, Map.empty, List.empty, List.empty)
}
