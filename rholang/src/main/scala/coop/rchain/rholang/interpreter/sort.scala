package coop.rchain.rholang.interpreter

import coop.rchain.rholang.intepreter._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn._

sealed trait Tree[T] {
  def size: Int
}
case class Leaf[T](item: T) extends Tree[T] {
  def size = 1
}
case class Node[T] (children: Seq[Tree[T]]) extends Tree[T] {
  def size = children.map(_.size).sum
}

object Leaves {
  // Shortcut to be able to write Leaves(1,2,3) instead of Node(Seq(Leaf(1),Leaf(2),Leaf(3)))
  def apply[T](children: T*) : Node[T] = Node(children.map(a => Leaf(a)))
}

case class ScoredTerm[T](term: T, score: Tree[Int]) extends Ordered[ScoredTerm[T]] {
  def compare(that: ScoredTerm[T]) : Int = {
    def compareScore(s1: Tree[Int], s2: Tree[Int]) : Int = {
      (s1, s2) match {
        case (Leaf(a), Leaf(b)) => a.compare(b)
        case (Node(h1 +: t1), Node(h2 +: t2)) =>
          compareScore(h1, h2) match {
            case 0 => compareScore(Node(t1), Node(t2))
            case other => other
          }
        case (Node(_), _) => -1
        case (_, Node(_)) => 1
      }
    }
    compareScore(this.score, that.score)
  }
}



