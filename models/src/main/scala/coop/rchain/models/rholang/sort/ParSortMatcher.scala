package coop.rchain.models.rholang.sort

import coop.rchain.models.Par

private[sort] object ParSortMatcher extends Sortable[Par] {
  def sortMatch(par: Par): ScoredTerm[Par] = {
    val sends       = par.sends.toList.map(s => Sortable.sortMatch(s)).sorted
    val receives    = par.receives.toList.map(r => Sortable.sortMatch(r)).sorted
    val exprs       = par.exprs.toList.map(e => Sortable.sortMatch(e)).sorted
    val news        = par.news.toList.map(n => Sortable.sortMatch(n)).sorted
    val matches     = par.matches.toList.map(m => Sortable.sortMatch(m)).sorted
    val bundles     = par.bundles.toList.map(b => Sortable.sortMatch(b)).sorted
    val connectives = par.connectives.toList.map(c => Sortable.sortMatch(c)).sorted
    val ids         = par.ids.map(g => ScoredTerm(g, Node(Score.PRIVATE, Leaf(g.id)))).sorted
    val sortedPar = Par(
      sends = sends.map(_.term),
      receives = receives.map(_.term),
      exprs = exprs.map(_.term),
      news = news.map(_.term),
      matches = matches.map(_.term),
      bundles = bundles.map(_.term),
      connectives = connectives.map(_.term),
      ids = ids.map(_.term),
      locallyFree = par.locallyFree,
      connectiveUsed = par.connectiveUsed
    )
    val parScore = Node(
      Score.PAR,
      sends.map(_.score) ++ receives.map(_.score) ++ exprs.map(_.score) ++ news
        .map(_.score) ++ matches.map(_.score) ++ bundles.map(_.score) ++ ids
        .map(_.score) ++ connectives.map(_.score): _*
    )
    ScoredTerm(sortedPar, parScore)
  }
}
