package coop.rchain

import com.google.protobuf.ByteString
import coop.rchain.metrics.Metrics

package object casper {

  type BlockHash = ByteString
  type Validator = ByteString

  val CasperMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "casper")
}
