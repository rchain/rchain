package coop.rchain

import coop.rchain.comm.transport.TransportLayerSyntax
import coop.rchain.metrics.Metrics

package object comm {
  val CommMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "comm")

  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxComm
}

// Comm syntax
trait AllSyntaxComm extends TransportLayerSyntax
