package coop.rchain.models.rholang.sort

import coop.rchain.models.{KeyValuePair, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sort.ScoredTerm._

object ordering {

  implicit class ListSortOps(ps: List[Par]) {
    def sort: List[Par] =
      ps.map(par => ParSortMatcher.sortMatch(par)).sorted.map(_.term)
  }

  implicit class MapSortOps(ps: Map[Par, Par]) {
    def sortKeyValuePair(key: Par, value: Par): ScoredTerm[(Par, Par)] = {
      val sortedKey   = ParSortMatcher.sortMatch(key)
      val sortedValue = ParSortMatcher.sortMatch(value)
      ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)
    }

    def sort: List[(Par, Par)] =
      ps.toList.map(kv => sortKeyValuePair(kv._1, kv._2)).sorted.map(_.term)
  }

}
