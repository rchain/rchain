package coop.rchain.rholang.interpreter

// This is an index map. Note that the internal environment is the same as the
// level map, but we calculate the correct index on get.
class DebruijnIndexMap[T](val next: Int, val env: Map[String, (Int, T, Int, Int)]) {
  def this() = this(0, Map[String, (Int, T, Int, Int)]())

  def newBinding(binding: (String, T, Int, Int)): DebruijnIndexMap[T] =
    binding match {
      case (varName, sort, line, col) =>
        DebruijnIndexMap[T](next + 1, env + (varName -> ((next, sort, line, col))))
    }

  // Returns the new map
  def newBindings(bindings: List[(String, T, Int, Int)]): DebruijnIndexMap[T] =
    (this /: bindings)((map, binding) => map.newBinding(binding))

  // Returns the new map, and a list of the shadowed variables
  // Takes a **Level** map, because we use that to track the Free Variables.
  def absorbFree(binders: DebruijnLevelMap[T]): (DebruijnIndexMap[T], List[(String, Int, Int)]) = {
    val finalNext  = next + binders.next
    val adjustNext = next
    binders.env.foldLeft((this, List[(String, Int, Int)]())) {
      case (
          (db: DebruijnIndexMap[T], shadowed: List[(String, Int, Int)]),
          (k: String, (level: Int, varType: T @unchecked, line: Int, col: Int))
          ) => {
        val shadowedNew = if (db.env.contains(k)) (k, line, col) :: shadowed else shadowed
        (
          DebruijnIndexMap(finalNext, db.env + (k -> ((level + adjustNext, varType, line, col)))),
          shadowedNew
        )
      }
    }
  }

  def getBinding(varName: String): Option[T] =
    for (pair <- env.get(varName)) yield pair._2
  def getLevel(varName: String): Option[Int] =
    for (pair <- env.get(varName)) yield pair._1
  def get(varName: String): Option[(Int, T, Int, Int)] =
    env
      .get(varName)
      .map({
        case (level, t, l, c) => (next - level - 1, t, l, c)
      })
  def isEmpty() = next == 0

  def count: Int = next

  override def equals(that: Any): Boolean =
    that match {
      case that: DebruijnLevelMap[T] =>
        next == that.next &&
          env == that.env
      case _ => false
    }

  override def hashCode(): Int =
    (next.hashCode() * 37 + env.hashCode)
}

object DebruijnIndexMap {
  def apply[T](next: Int, env: Map[String, (Int, T, Int, Int)]): DebruijnIndexMap[T] =
    new DebruijnIndexMap(next, env)

  def apply[T](): DebruijnIndexMap[T] = new DebruijnIndexMap[T]()

  def unapply[T](db: DebruijnIndexMap[T]): Option[(Int, Map[String, (Int, T, Int, Int)])] =
    Some((db.next, db.env))
}
