package coop.rchain

import coop.rchain.metrics.Metrics
import coop.rchain.rspace.history.syntax.HistoryRepositorySyntax
import coop.rchain.rspace.state.RSpaceExporterSyntax
import coop.rchain.rspace.store.RSpaceStoreManagerSyntax

package object rspace {
  val RSpaceMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "rspace")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxRSpace
}
trait AllSyntaxRSpace
    extends RSpaceExporterSyntax
    with HistoryRepositorySyntax
    with RSpaceStoreManagerSyntax
