package coop.rchain

import coop.rchain.metrics.Metrics
import coop.rchain.rholang.interpreter.{RhoHistoryRepositorySyntax, RhoRuntimeSyntax}

package object rholang {
  val RholangMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "rholang")

  object syntax extends AllSyntaxRholang
}

trait AllSyntaxRholang extends RhoRuntimeSyntax with RhoHistoryRepositorySyntax
