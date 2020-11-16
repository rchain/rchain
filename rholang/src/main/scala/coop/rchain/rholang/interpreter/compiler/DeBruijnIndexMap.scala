package coop.rchain.rholang.interpreter.compiler

// This is an index map. Note that the internal environment is the same as the
// level map, but we calculate the correct index on get.
final case class DeBruijnIndexMap[T](next: Int, env: Map[String, IndexContext[T]]) {

  def put(binding: IdContext[T]): DeBruijnIndexMap[T] =
    binding match {
      case (varName, sort, sourcePosition) =>
        DeBruijnIndexMap(next + 1, env + (varName -> IndexContext(next, sort, sourcePosition)))
    }

  // Returns the new map
  def put(bindings: List[IdContext[T]]): DeBruijnIndexMap[T] =
    bindings.foldLeft(this)((map, binding) => map.put(binding))

  // Returns the new map, and a list of the shadowed variables
  // Takes a **Level** map, because we use that to track the Free Variables.
  def absorbFree(binders: DeBruijnLevelMap[T]): DeBruijnIndexMap[T] =
    DeBruijnIndexMap(
      next + binders.next,
      binders.env.foldLeft(env) {
        case (accEnv, (name, LevelContext(level, typ, sourcePosition))) =>
          accEnv + (name -> IndexContext(level + next, typ, sourcePosition))
      }
    )

  def get(varName: String): Option[IndexContext[T]] =
    env.get(varName).map {
      case IndexContext(level, t, sourcePosition) =>
        IndexContext(next - level - 1, t, sourcePosition)
    }

  def count: Int = next

}

object DeBruijnIndexMap {
  def empty[T]: DeBruijnIndexMap[T] = DeBruijnIndexMap[T](0, Map.empty)
}
