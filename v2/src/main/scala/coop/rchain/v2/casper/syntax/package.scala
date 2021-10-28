package coop.rchain.v2.casper

package object syntax {
  object all
      extends DependencyGraphSyntax
      with LatestMessagesSyntax
      with FinalizationFringeSyntax
      with stcasper.syntax.StateMessageSyntax
}
