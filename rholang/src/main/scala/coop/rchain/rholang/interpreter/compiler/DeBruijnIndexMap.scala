package coop.rchain.rholang.interpreter.compiler

/**
  *
  * A structure for keeping track of bound variables. The internal environment is the same as
  * DeBruijnLevelMap, but here we calculate the correct index on get. This way, we don't have to
  * reindex the map.
  *
  * @param nextIndex The DeBruijn index assigned to the next variable added to the map.
  * @param indexBindings A map of names to DeBruijn indices.
  * @tparam T The typing discipline we're enforcing
  */
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

  def put(bindings: List[IdContext[T]]): DeBruijnIndexMap[T] =
    bindings.foldLeft(this)((indexMap, binding) => indexMap.put(binding))

  // Takes a level map because we use that to track the free Variables.
  def absorbFree(levelMap: FreeMap[T]): DeBruijnIndexMap[T] =
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
