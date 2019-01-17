package coop.rchain

import com.google.protobuf.ByteString
import coop.rchain.metrics.Metrics

package object casper {

  type BlockHash = ByteString

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
}
