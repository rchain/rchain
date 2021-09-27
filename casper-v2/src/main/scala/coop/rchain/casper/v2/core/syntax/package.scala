package coop.rchain.casper.v2.core

package object syntax {
  object all extends DependencyGraphSyntax with SafetyOracleSyntax with CasperSyntax
}
