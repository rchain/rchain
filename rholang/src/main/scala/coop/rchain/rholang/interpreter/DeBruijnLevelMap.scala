package coop.rchain.rholang.interpreter
import coop.rchain.models.Connective.ConnectiveInstance

// Parameterized over T, the kind of typing discipline we are enforcing.

// An index map is implemented as a level map that calculates the index on get.
// This way you don't have to re-number the map, you just calculate the index on
// get.
// Parameterized over T, the kind of typing discipline we are enforcing.
final case class DeBruijnLevelMap[T](
    next: Int,
    env: Map[String, (Int, T, Int, Int)],
    wildcards: List[(Int, Int)],
    logicalConnectives: List[(ConnectiveInstance, Int, Int)]
) {

  def newBinding(binding: (String, T, Int, Int)): (DeBruijnLevelMap[T], Int) =
    binding match {
      case (varName, sort, line, col) =>
        (
          DeBruijnLevelMap[T](
            next + 1,
            env + (varName -> ((next, sort, line, col))),
            wildcards,
            logicalConnectives
          ),
          next
        )
    }

  // Returns the new map, and the first value assigned. Given that they're assigned contiguously
  def newBindings(bindings: List[(String, T, Int, Int)]): (DeBruijnLevelMap[T], Int) = {
    val newMap = bindings.foldLeft(this)((map, binding) => map.newBinding(binding)._1)
    (newMap, next)
  }

  // Returns the new map, and a list of the shadowed variables
  def merge(binders: DeBruijnLevelMap[T]): (DeBruijnLevelMap[T], List[(String, Int, Int)]) = {
    val finalNext        = next + binders.next
    val finalWildcards   = wildcards ++ binders.wildcards
    val finalConnectives = logicalConnectives ++ binders.logicalConnectives
    val adjustNext       = next
    binders.env.foldLeft((this, List[(String, Int, Int)]())) {
      case (
          (db: DeBruijnLevelMap[T], shadowed: List[(String, Int, Int)]),
          (k: String, (level: Int, varType: T @unchecked, line: Int, col: Int))
          ) =>
        val shadowedNew = if (db.env.contains(k)) (k, line, col) :: shadowed else shadowed
        (
          DeBruijnLevelMap(
            finalNext,
            db.env + (k -> ((level + adjustNext, varType, line, col))),
            finalWildcards,
            finalConnectives
          ),
          shadowedNew
        )
    }
  }

  // Returns the new map
  def addWildcard(line: Int, col: Int): DeBruijnLevelMap[T] = {
    val newWildcards: List[(Int, Int)] = wildcards :+ ((line, col))
    DeBruijnLevelMap(next, env, newWildcards, logicalConnectives)
  }

  def addLogicalConnective[C <: ConnectiveInstance](
      connective: C,
      line: Int,
      col: Int
  ): DeBruijnLevelMap[T] = {
    val newConnectives = logicalConnectives :+ ((connective, line, col))
    DeBruijnLevelMap(next, env, wildcards, newConnectives)
  }

  def get(varName: String): Option[(Int, T, Int, Int)] = env.get(varName)

  def count: Int            = next + wildcards.length + logicalConnectives.length
  def countNoWildcards: Int = next
}

object DeBruijnLevelMap {
  def empty[T]: DeBruijnLevelMap[T] = DeBruijnLevelMap[T](0, Map.empty, List.empty, List.empty)
}
