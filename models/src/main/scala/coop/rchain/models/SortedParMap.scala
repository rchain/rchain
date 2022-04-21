package coop.rchain.models

import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.SortedParMap._
import coop.rchain.models.rholang.sorter._
import coop.rchain.models.rholang.sorter.ordering.MapSortOpsFast
import monix.eval.Coeval

final class SortedParMap private (ps: Map[Par, (Tree[ScoreAtom], Par)], isComplexMap: Boolean)
    extends Iterable[(Par, Par)] {

  private lazy val psList: List[(Par, (Tree[ScoreAtom], Par))] = ps.toList

  lazy val unsortedList: List[(Par, Par)] = psList.map(x => (x._1, x._2._2))

  private lazy val sortedSTList: List[ScoredTerm[(Par, Par)]] = psList.sort

  lazy val sortedScoreList: List[Tree[ScoreAtom]] = sortedSTList.map(_.score)

  lazy val sortedList: List[(Par, Par)] = sortedSTList.map {
    case ScoredTerm((keyTerm, valueTerm), _) => (keyTerm, valueTerm)
  }

  def +(kv: (Par, Par)): SortedParMap =
    new SortedParMap(ps + sortKV(kv), isComplexMap || isComplexKV(kv))

  def ++(kvs: Seq[(Par, Par)]): SortedParMap =
    new SortedParMap(ps ++ kvs.map(sortKV), isComplexMap || kvs.exists(isComplexKV))

  def ++(sortedMap: SortedParMap): SortedParMap =
    new SortedParMap(ps ++ sortedMap.getPs, isComplexMap || sortedMap.isComplex)

  def -(key: Par): SortedParMap = new SortedParMap(ps - sortPar(key), isComplexMap)
  // TODO: If isComplexMap == true it's possible true explore again

  def --(keys: Seq[Par]): SortedParMap = new SortedParMap(ps -- keys.map(sortPar), isComplexMap)
  // TODO: If isComplexMap == true it's possible true explore again

  def --(sortedMap: SortedParMap): SortedParMap =
    new SortedParMap(ps -- sortedMap.unsortedKeys, isComplexMap)
  // TODO: If isComplexMap == true it's possible true explore again

  def apply(par: Par): Par = ps(sortPar(par))._2

  def contains(par: Par): Boolean = ps.contains(sortPar(par))

  def empty: SortedParMap = SortedParMap.empty

  def get(key: Par): Option[Par] = ps.get(sortPar(key)).map(_._2)

  def getOrElse(key: Par, default: Par): Par = get(key).getOrElse(default)

  def unsortedKeys: Iterable[Par] = psList.map(_._1)

  def unsortedValues: Iterable[Par] = psList.map(_._2._2)

  def sortedKeys: Iterable[Par] = sortedSTList.map {
    case ScoredTerm((keyTerm, _), _) => keyTerm
  }

  def sortedValues: Iterable[Par] = sortedSTList.map {
    case ScoredTerm((_, valueTerm), _) => valueTerm
  }

  def iterator: Iterator[(Par, Par)] = sortedList.toIterator

  override def equals(that: Any): Boolean = that match {
    case spm: SortedParMap => spm.getPs == ps
    case _                 => false
  }

  override def hashCode(): Int = sortedList.hashCode()

  def getPs: Map[Par, (Tree[ScoreAtom], Par)] = ps

  def isComplex: Boolean = isComplexMap
}

object SortedParMap {

  def apply(seq: Seq[(Par, Par)]): SortedParMap =
    new SortedParMap(seq.map(sortKV).toMap, seq.exists(isComplexKV))

  def apply(map: Map[Par, Par]): SortedParMap = apply(map.toSeq)

  def empty: SortedParMap = new SortedParMap(Map.empty[Par, (Tree[ScoreAtom], Par)], false)

  private def sort(par: Par): (Par, Tree[ScoreAtom]) =
    Sortable[Par].sortMatch[Coeval](par).map(x => (x.term, x.score)).value()

  private def sortPar(par: Par): Par = Sortable[Par].sortMatch[Coeval](par).map(_.term).value()

  private def sortKV(kv: (Par, Par)): (Par, (Tree[ScoreAtom], Par)) = {
    val (sortedKeyTerm, sortedKeyScore) = sort(kv._1)
    val sortedValueTerm                 = sortPar(kv._2)
    (sortedKeyTerm, (sortedKeyScore, sortedValueTerm))
  }

  private def isComplexKV(kv: (Par, Par)): Boolean = isComplexPar(kv._1) || isComplexPar(kv._2)

  private def isComplexPar(par: Par): Boolean =
    par match {
      case Par(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), _, _) => false
      case Par(Seq(), Seq(), Seq(), exprs, Seq(), Seq(), Seq(), Seq(), _, _) =>
        exprs.exists(isComplexExpr)
      case _ => true
    }

  private def isComplexExpr(expr: Expr): Boolean =
    expr.exprInstance match {
      case _: GBool      => false
      case _: GInt       => false
      case _: GString    => false
      case _: GUri       => false
      case _: GByteArray => false
      // TODO: Need explore EListBody, ETupleBody, ESetBody, EMapBody
      case _ => true
    }

}
