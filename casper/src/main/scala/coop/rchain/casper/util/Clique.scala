package coop.rchain.casper.util

object Clique {
  // e is a list of undirected edges
  def findCliquesRecursive[A](e: List[(A, A)]): Stream[List[A]] = {
    val adj = getAdj(e)

    def expand(ans: List[A], P: Set[A], X: Set[A]): Stream[List[A]] =
      if (P.isEmpty && X.isEmpty) {
        if (ans.isEmpty)
          Stream.empty[List[A]]
        else
          Stream(ans)
      } else {
        val u     = (P ++ X).maxBy(v => (P & adj.getOrElse(v, Set())).size)
        val P_ext = P &~ adj.getOrElse(u, Set())
        val runTimeParameter = P_ext.toList
          .scanLeft((Set.empty[A], None: Option[A])) {
            case ((acc, _), elem) => (acc + elem, Some(elem))
          }
          .tail

        runTimeParameter.toStream.flatMap {
          case (s, Some(elem)) => {
            val adjQ = adj.getOrElse(elem, Set())
            expand(ans :+ elem, (P &~ s) & adjQ, (P ++ s) & adjQ)
          }
        }
      }

    expand(List(), getNodeSet(e), Set())
  }

  // e is a list of undirected edges
  private def getNodeSet[A](e: List[(A, A)]): Set[A] =
    e.flatMap(it => List(it._1, it._2)).toSet

  // e is a list of undirected edges
  private def getAdj[A](e: List[(A, A)]): Map[A, Set[A]] = {
    val directedEdges: Seq[(A, A)] = e ++ e.map(it => (it._2, it._1))

    directedEdges.groupBy(_._1).map {
      case (k, v) => k -> v.filter(e => e._1 != e._2).map(_._2).toSet
    }
  }
}
