package coop.rchain.blockstorage.casper

package object syntax {
  object all extends DependencyGraphSyntax with SafetyOracleSyntax with CasperSyntax
}
