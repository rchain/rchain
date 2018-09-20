package coop.rchain.node.diagnostics

import java.lang.management.ManagementFactory
import java.time.Duration
import java.util.concurrent.atomic.AtomicReference

import com.typesafe.config.Config
import javax.management.ObjectName
import kamon.MetricReporter
import kamon.metric._
import java.time.Instant

class JmxReporter extends MetricReporter {

  private val snapshotAccumulator =
    new PeriodSnapshotAccumulator(Duration.ofDays(365 * 5), Duration.ZERO)

  private val preparedResult =
    new AtomicReference[NodeMetricsSnapshotBean](NodeMetricsSnapshotBean())

  private val mbs      = ManagementFactory.getPlatformMBeanServer
  private val beanName = ObjectName.getInstance(NodeMXBean.Name)
  private val nodeBean = new NodeMetricsBean(preparedResult)

  def start(): Unit =
    mbs.registerMBean(nodeBean, beanName)

  def stop(): Unit =
    mbs.unregisterMBean(beanName)

  def reconfigure(config: Config): Unit = ()

  def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    snapshotAccumulator.add(snapshot)
    val currentData = snapshotAccumulator.peek()
    preparedResult.set(NodeMetricsSnapshotBean(currentData))
  }

}

private[diagnostics] class NodeMetricsSnapshotBean(snapshot: PeriodSnapshot) extends NodeMXBean {

  private lazy val counters = toMap(snapshot.metrics.counters)
  private lazy val gauges   = toMap(snapshot.metrics.gauges)

  private def toMap(metricValues: Seq[MetricValue]): Map[String, MetricValue] =
    metricValues.map(v => v.name -> v).toMap

  private def getValue(name: String, metricValues: Map[String, MetricValue]): Long =
    metricValues.get(name).map(_.value).getOrElse(0L)

  lazy val getPingReceiverCount: Long =
    getValue("ping-recv-count", counters)

  lazy val getLookupReceiverCount: Long =
    getValue("lookup-recv-count", counters)

  lazy val getDisconnectReceiverCount: Long =
    getValue("disconnect-recv-count", counters)

  lazy val getConnects: Long =
    getValue("connects", counters)

  lazy val getP2pEncryptionHandshakeReceiverCount: Long =
    getValue("p2p-encryption-handshake-recv-count", counters)

  lazy val getP2pProtocolHandshakeReceiverCount: Long =
    getValue("p2p-protocol-handshake-recv-count", counters)

  lazy val getPeers: Long =
    getValue("peers", gauges)

  lazy val getFrom: Long = snapshot.from.toEpochMilli

  lazy val getTo: Long = snapshot.to.toEpochMilli

}

private[diagnostics] object NodeMetricsSnapshotBean {
  def apply(): NodeMetricsSnapshotBean =
    new NodeMetricsSnapshotBean(
      PeriodSnapshot(Instant.EPOCH, Instant.EPOCH, MetricsSnapshot(Nil, Nil, Nil, Nil))
    )
  def apply(metrics: PeriodSnapshot): NodeMetricsSnapshotBean =
    new NodeMetricsSnapshotBean(metrics)
}

private[diagnostics] class NodeMetricsBean(bean: AtomicReference[NodeMetricsSnapshotBean])
    extends NodeMXBean {
  def getPingReceiverCount: Long                   = bean.get.getPingReceiverCount
  def getLookupReceiverCount: Long                 = bean.get.getLookupReceiverCount
  def getDisconnectReceiverCount: Long             = bean.get.getDisconnectReceiverCount
  def getConnects: Long                            = bean.get.getConnects
  def getP2pEncryptionHandshakeReceiverCount: Long = bean.get.getP2pEncryptionHandshakeReceiverCount
  def getP2pProtocolHandshakeReceiverCount: Long   = bean.get.getP2pProtocolHandshakeReceiverCount
  def getPeers: Long                               = bean.get.getPeers
  def getFrom: Long                                = bean.get.getFrom
  def getTo: Long                                  = bean.get.getTo
}
