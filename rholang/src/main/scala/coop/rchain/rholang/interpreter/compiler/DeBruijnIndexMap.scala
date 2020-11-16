package coop.rchain.rholang.interpreter.compiler

// This is an index map. Note that the internal environment is the same as the
// level map, but we calculate the correct index on get.
final case class DeBruijnIndexMap[T](nextIndex: Int, indexBindings: Map[String, IndexContext[T]]) {

  def get(name: String): Option[IndexContext[T]] =
    indexBindings.get(name).map {
      case IndexContext(level, typ, sourcePosition) =>
        IndexContext(nextIndex - level - 1, typ, sourcePosition)
    }

  def put(binding: IdContext[T]): DeBruijnIndexMap[T] =
    binding match {
      case (name, typ, sourcePosition) =>
        DeBruijnIndexMap(
          nextIndex + 1,
          indexBindings + (name -> IndexContext(nextIndex, typ, sourcePosition))
        )
    }

  // Returns the new map
  def put(bindings: List[IdContext[T]]): DeBruijnIndexMap[T] =
    bindings.foldLeft(this)((indexMap, binding) => indexMap.put(binding))

  // Returns the new map, and a list of the shadowed variables
  // Takes a **Level** map, because we use that to track the Free Variables.
  def absorbFree(levelMap: DeBruijnLevelMap[T]): DeBruijnIndexMap[T] =
    DeBruijnIndexMap(
      nextIndex + levelMap.nextLevel,
      levelMap.levelBindings.foldLeft(indexBindings) {
        case (accEnv, (name, LevelContext(level, typ, sourcePosition))) =>
          accEnv + (name -> IndexContext(level + nextIndex, typ, sourcePosition))
      }
    )

  def count: Int = nextIndex

}

object DeBruijnIndexMap {
  def empty[T]: DeBruijnIndexMap[T] = DeBruijnIndexMap[T](0, Map.empty)
}
