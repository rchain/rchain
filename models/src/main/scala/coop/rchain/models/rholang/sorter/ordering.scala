package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.models.rholang.sorter.ScoredTerm._
import monix.eval.Coeval
import cats.implicits._

//FIXME the `.sort` methods in this file should return via F[_] : Sync, and the corresponding ParSet and ParMap should
//be constructed via factory methods also returning via F. Otherwise we risk StackOverflowErrors.
object ordering {

  implicit class ListSortOps(ps: List[Par]) {
    implicit val sync = implicitly[Sync[Coeval]]

    def sort: List[Par] = {
      val psSorted: List[Coeval[ScoredTerm[Par]]] =
        ps.map(par => Sortable[Par].sortMatch[Coeval](par))
      val coeval: Coeval[List[Par]] = for {
        parsSorted <- psSorted.sequence
      } yield parsSorted.sorted.map(_.term)

      coeval.value
    }
  }

  implicit class MapSortOps(ps: Map[Par, Par]) {
    implicit val sync = implicitly[Sync[Coeval]]

    def sortKeyValuePair(key: Par, value: Par): Coeval[ScoredTerm[(Par, Par)]] =
      for {
        sortedKey   <- Sortable.sortMatch(key)
        sortedValue <- Sortable.sortMatch(value)
      } yield ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)

    def sort: List[(Par, Par)] = {
      val pairsSorted = ps.toList.map(kv => sortKeyValuePair(kv._1, kv._2))
      val coeval: Coeval[List[(Par, Par)]] = for {
        sequenced <- pairsSorted.sequence
      } yield sequenced.sorted.map(_.term)
      coeval.value
    }
  }

  implicit class MapSortOpsFast(ps: List[(Par, (Tree[ScoreAtom], Par))]) {
    implicit val sync = implicitly[Sync[Coeval]]

    // TODO: It's needed to use Coeval in this case?
    def sort: List[ScoredTerm[(Par, Par)]] = {
      val pairsSorted: List[Coeval[ScoredTerm[(Par, Par)]]] = ps.map {
        case (keyTerm, (keyScore, valueTerm)) =>
          Coeval.now { ScoredTerm((keyTerm, valueTerm), keyScore) }
      }
      val coeval: Coeval[List[ScoredTerm[(Par, Par)]]] = for {
        sequenced <- pairsSorted.sequence
      } yield sequenced.sorted
      coeval.value
    }
  }

}
