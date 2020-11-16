package coop.rchain.rholang.interpreter.compiler

// This is an index map. Note that the internal environment is the same as the
// level map, but we calculate the correct index on get.
final case class DeBruijnIndexMap[T](next: Int, env: Map[String, (Int, T, SourcePosition)]) {

  def put(binding: (String, T, SourcePosition)): DeBruijnIndexMap[T] =
    binding match {
      case (varName, sort, sourcePosition) =>
        DeBruijnIndexMap[T](next + 1, env + (varName -> ((next, sort, sourcePosition))))
    }

  // Returns the new map
  def put(bindings: List[(String, T, SourcePosition)]): DeBruijnIndexMap[T] =
    bindings.foldLeft(this)((map, binding) => map.put(binding))

  // Returns the new map, and a list of the shadowed variables
  // Takes a **Level** map, because we use that to track the Free Variables.
  def absorbFree(
      binders: DeBruijnLevelMap[T]
  ): (DeBruijnIndexMap[T], List[(String, SourcePosition)]) = {
    val finalNext  = next + binders.next
    val adjustNext = next
    binders.env.foldLeft((this, List[(String, SourcePosition)]())) {
      case (
          (db: DeBruijnIndexMap[T], shadowed: List[(String, SourcePosition)]),
          (k: String, (level: Int, varType: T @unchecked, sourcePosition))
          ) => {
        val shadowedNew = if (db.env.contains(k)) (k, sourcePosition) :: shadowed else shadowed
        (
          DeBruijnIndexMap(
            finalNext,
            db.env + (k -> ((level + adjustNext, varType, sourcePosition)))
          ),
          shadowedNew
        )
      }
    }
  }

  def get(varName: String): Option[(Int, T, SourcePosition)] =
    env
      .get(varName)
      .map({
        case (level, t, sourcePosition) => (next - level - 1, t, sourcePosition)
      })

  def count: Int = next

}

object DeBruijnIndexMap {
  def empty[T]: DeBruijnIndexMap[T] = DeBruijnIndexMap[T](0, Map.empty)
}
