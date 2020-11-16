package coop.rchain.rholang.interpreter.compiler

import coop.rchain.models.Connective.ConnectiveInstance

// Parameterized over T, the kind of typing discipline we are enforcing.

// An index map is implemented as a level map that calculates the index on get.
// This way you don't have to re-number the map, you just calculate the index on
// get.
// Parameterized over T, the kind of typing discipline we are enforcing.
final case class DeBruijnLevelMap[T](
    nextLevel: Int,
    levelBindings: Map[String, LevelContext[T]],
    wildcards: List[SourcePosition],
    connectives: List[(ConnectiveInstance, SourcePosition)]
) {

  def get(name: String): Option[LevelContext[T]] = levelBindings.get(name)

  def put(binding: IdContext[T]): DeBruijnLevelMap[T] =
    binding match {
      case (name, typ, sourcePosition) =>
        DeBruijnLevelMap[T](
          nextLevel + 1,
          levelBindings + (name -> LevelContext(nextLevel, typ, sourcePosition)),
          wildcards,
          connectives
        )
    }

  // Returns the new map, and the first value assigned. Given that they're assigned contiguously
  def put(bindings: List[IdContext[T]]): DeBruijnLevelMap[T] =
    bindings.foldLeft(this)((levelMap, binding) => levelMap.put(binding))

  // Returns the new map, and a list of the shadowed variables
  def merge(freeMap: DeBruijnLevelMap[T]): (DeBruijnLevelMap[T], List[(String, SourcePosition)]) = {

    val (accEnv, shadowed) =
      freeMap.levelBindings.foldLeft((levelBindings, List.empty[(String, SourcePosition)])) {
        case ((accEnv, shadowed), (name, LevelContext(level, typ, sourcePosition))) =>
          (
            accEnv + (name -> LevelContext(level + nextLevel, typ, sourcePosition)),
            if (levelBindings.contains(name)) (name, sourcePosition) :: shadowed
            else shadowed
          )
      }

    (
      DeBruijnLevelMap(
        nextLevel + freeMap.nextLevel,
        accEnv,
        wildcards ++ freeMap.wildcards,
        connectives ++ freeMap.connectives
      ),
      shadowed
    )
  }

  def addWildcard(sourcePosition: SourcePosition): DeBruijnLevelMap[T] =
    DeBruijnLevelMap(nextLevel, levelBindings, wildcards :+ sourcePosition, connectives)

  def addConnective(
      connective: ConnectiveInstance,
      sourcePosition: SourcePosition
  ): DeBruijnLevelMap[T] =
    DeBruijnLevelMap(
      nextLevel,
      levelBindings,
      wildcards,
      connectives :+ (connective, sourcePosition)
    )

  def count: Int = nextLevel + wildcards.length + connectives.length

  def countNoWildcards: Int = nextLevel

}

object DeBruijnLevelMap {
  def empty[T]: DeBruijnLevelMap[T] = DeBruijnLevelMap[T](0, Map.empty, List.empty, List.empty)
}
