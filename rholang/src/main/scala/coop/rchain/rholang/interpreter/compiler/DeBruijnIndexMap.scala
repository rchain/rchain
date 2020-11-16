package coop.rchain.rholang.interpreter.compiler

// This is an index map. Note that the internal environment is the same as the
// level map, but we calculate the correct index on get.
final case class DeBruijnIndexMap[T](next: Int, env: Map[String, (Int, T, Int, Int)]) {

  def put(binding: (String, T, Int, Int)): DeBruijnIndexMap[T] =
    binding match {
      case (varName, sort, line, col) =>
        DeBruijnIndexMap[T](next + 1, env + (varName -> ((next, sort, line, col))))
    }

  // Returns the new map
  def put(bindings: List[(String, T, Int, Int)]): DeBruijnIndexMap[T] =
    bindings.foldLeft(this)((map, binding) => map.put(binding))

  // Returns the new map, and a list of the shadowed variables
  // Takes a **Level** map, because we use that to track the Free Variables.
  def absorbFree(binders: DeBruijnLevelMap[T]): (DeBruijnIndexMap[T], List[(String, Int, Int)]) = {
    val finalNext  = next + binders.next
    val adjustNext = next
    binders.env.foldLeft((this, List[(String, Int, Int)]())) {
      case (
          (db: DeBruijnIndexMap[T], shadowed: List[(String, Int, Int)]),
          (k: String, (level: Int, varType: T @unchecked, line: Int, col: Int))
          ) => {
        val shadowedNew = if (db.env.contains(k)) (k, line, col) :: shadowed else shadowed
        (
          DeBruijnIndexMap(finalNext, db.env + (k -> ((level + adjustNext, varType, line, col)))),
          shadowedNew
        )
      }
    }
  }

  def get(varName: String): Option[(Int, T, Int, Int)] =
    env
      .get(varName)
      .map({
        case (level, t, l, c) => (next - level - 1, t, l, c)
      })

  def count: Int = next

}

object DeBruijnIndexMap {
  def empty[T]: DeBruijnIndexMap[T] = DeBruijnIndexMap[T](0, Map.empty)
}
