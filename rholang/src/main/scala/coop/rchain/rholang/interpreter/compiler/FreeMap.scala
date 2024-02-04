package coop.rchain.rholang.interpreter.compiler

import coop.rchain.models.rholangn._

/**
  * A structure to keep track of free variables using de Bruijn levels (0 based).
  *
  * @param nextLevel The de Bruijn level assigned to the next variable name added to the map.
  * @param levelBindings A map of names to DeBruijn levels.
  * @param wildcards A list of the positions of _ patterns.
  * @param connectives A list of the positions of logical connectives.
  * @tparam T The typing discipline we're enforcing.
  */
final case class FreeMap[T](
    nextLevel: Int,
    levelBindings: Map[String, FreeContext[T]],
    wildcards: List[SourcePosition],
    connectives: List[(ConnectiveN, SourcePosition)]
) {

  def get(name: String): Option[FreeContext[T]] = levelBindings.get(name)

  def put(binding: IdContext[T]): FreeMap[T] =
    binding match {
      case (name, typ, sourcePosition) =>
        FreeMap[T](
          nextLevel + 1,
          levelBindings + (name -> FreeContext(nextLevel, typ, sourcePosition)),
          wildcards,
          connectives
        )
    }

  def put(bindings: List[IdContext[T]]): FreeMap[T] =
    bindings.foldLeft(this)((levelMap, binding) => levelMap.put(binding))

  // Returns the new map, and a list of the shadowed variables
  def merge(freeMap: FreeMap[T]): (FreeMap[T], List[(String, SourcePosition)]) = {

    val (accEnv, shadowed) =
      freeMap.levelBindings.foldLeft((levelBindings, List.empty[(String, SourcePosition)])) {
        case ((accEnv, shadowed), (name, FreeContext(level, typ, sourcePosition))) =>
          (
            accEnv + (name -> FreeContext(level + nextLevel, typ, sourcePosition)),
            if (levelBindings.contains(name)) (name, sourcePosition) :: shadowed
            else shadowed
          )
      }

    (
      FreeMap(
        nextLevel + freeMap.nextLevel,
        accEnv,
        wildcards ++ freeMap.wildcards,
        connectives ++ freeMap.connectives
      ),
      shadowed
    )
  }

  def addWildcard(sourcePosition: SourcePosition): FreeMap[T] =
    FreeMap(nextLevel, levelBindings, wildcards :+ sourcePosition, connectives)

  def addConnective(
      connective: ConnectiveN,
      sourcePosition: SourcePosition
  ): FreeMap[T] =
    FreeMap(
      nextLevel,
      levelBindings,
      wildcards,
      connectives :+ ((connective, sourcePosition))
    )

  def count: Int = nextLevel + wildcards.length + connectives.length

  def countNoWildcards: Int = nextLevel

}

object FreeMap {
  def empty[T]: FreeMap[T] = FreeMap[T](0, Map.empty, List.empty, List.empty)
}
