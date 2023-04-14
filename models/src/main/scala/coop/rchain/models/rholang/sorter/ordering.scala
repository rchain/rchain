package coop.rchain.models.rholang.sorter

import cats.effect.{Sync}
import coop.rchain.models.Par
import coop.rchain.models.rholang.sorter.ScoredTerm._
import cats.Eval
import cats.effect.kernel.Resource.ExitCase
import cats.implicits._
import coop.rchain.catscontrib.effect.implicits.sEval

//FIXME the `.sort` methods in this file should return via F[_] : Sync, and the corresponding ParSet and ParMap should
//be constructed via factory methods also returning via F. Otherwise we risk StackOverflowErrors.
object ordering {

  implicit class ListSortOps(ps: List[Par]) {

    def sort: List[Par] = {
      val psSorted: List[Eval[ScoredTerm[Par]]] =
        ps.map(par => Sortable[Par].sortMatch[Eval](par))
      val eval: Eval[List[Par]] = for {
        parsSorted <- psSorted.sequence
      } yield parsSorted.sorted.map(_.term)

      eval.value
    }
  }

  implicit class MapSortOps(ps: Map[Par, Par]) {

    def sortKeyValuePair(key: Par, value: Par): Eval[ScoredTerm[(Par, Par)]] =
      for {
        sortedKey   <- Sortable.sortMatch(key)
        sortedValue <- Sortable.sortMatch(value)
      } yield ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)

    def sort: List[(Par, Par)] = {
      val pairsSorted = ps.toList.map(kv => sortKeyValuePair(kv._1, kv._2))
      val eval: Eval[List[(Par, Par)]] = for {
        sequenced <- pairsSorted.sequence
      } yield sequenced.sorted.map(_.term)
      eval.value
    }
  }

}
