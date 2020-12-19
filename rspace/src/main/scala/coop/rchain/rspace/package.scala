package coop.rchain

import coop.rchain.metrics.Metrics
import coop.rchain.rspace.state.RSpaceExporterSyntax

package object rspace {
  val RSpaceMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "rspace")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxRSpace
}

trait AllSyntaxRSpace extends RSpaceExporterSyntax
