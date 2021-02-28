package coop.rchain.rholang.interpreter.compiler

/**
  * A structure for keeping track of bound variables using de Bruijn indices. The internal environment is the same as
  * FreeMap, but here we calculate the correct index on get. This way, we don't have to reindex the map.
  *
  * @param nextIndex The de Bruijn index assigned to the next variable added to the map.
  * @param indexBindings A map of names to DeBruijn indices.
  * @tparam T The typing discipline we're enforcing
  */
final case class BoundMap[T](nextIndex: Int, indexBindings: Map[String, BoundContext[T]]) {

  def get(name: String): Option[BoundContext[T]] =
    indexBindings.get(name).map {
      case BoundContext(level, typ, sourcePosition) =>
        BoundContext(nextIndex - level - 1, typ, sourcePosition)
    }

  def put(binding: IdContext[T]): BoundMap[T] =
    binding match {
      case (name, typ, sourcePosition) =>
        BoundMap(
          nextIndex + 1,
          indexBindings + (name -> BoundContext(nextIndex, typ, sourcePosition))
        )
    }

  def put(bindings: List[IdContext[T]]): BoundMap[T] =
    bindings.foldLeft(this)((indexMap, binding) => indexMap.put(binding))

  def absorbFree(levelMap: FreeMap[T]): BoundMap[T] =
    BoundMap(
      nextIndex + levelMap.nextLevel,
      levelMap.levelBindings.foldLeft(indexBindings) {
        case (accEnv, (name, FreeContext(level, typ, sourcePosition))) =>
          accEnv + (name -> BoundContext(level + nextIndex, typ, sourcePosition))
      }
    )

  def count: Int = nextIndex

}

object BoundMap {
  def empty[T]: BoundMap[T] = BoundMap[T](0, Map.empty)
}
