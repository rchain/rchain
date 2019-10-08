package coop.rchain

import coop.rchain.metrics.Metrics

package object rholang {
  val RholangMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "rholang")
}
