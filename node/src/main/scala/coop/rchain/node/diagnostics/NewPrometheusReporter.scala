package coop.rchain.node.diagnostics

import java.time.Duration

import com.typesafe.config.{Config, ConfigUtil}
import coop.rchain.node.Ok
import kamon._
import kamon.metric._
import monix.eval.Task
import org.http4s._
import org.http4s.dsl.io._

import scala.collection.JavaConverters._

/**
  * Based on kamon-prometheus but without the embedded server
  */
class NewPrometheusReporter extends MetricReporter {
  import NewPrometheusReporter.Configuration.{environmentTags, readConfiguration}

  private val snapshotAccumulator =
    new PeriodSnapshotAccumulator(Duration.ofDays(365 * 5), Duration.ZERO)

  @volatile private var preparedScrapeData: String =
    "# The kamon-prometheus module didn't receive any data just yet.\n"

  override def start(): Unit                        = {}
  override def stop(): Unit                         = {}
  override def reconfigure(newConfig: Config): Unit = {}

  override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    snapshotAccumulator.add(snapshot)
    val currentData           = snapshotAccumulator.peek()
    val reporterConfiguration = readConfiguration(Kamon.config())
    val scrapeDataBuilder =
      new ScrapeDataBuilder(reporterConfiguration, environmentTags(reporterConfiguration))

    scrapeDataBuilder.appendCounters(currentData.metrics.counters)
    scrapeDataBuilder.appendGauges(currentData.metrics.gauges)
    scrapeDataBuilder.appendHistograms(currentData.metrics.histograms)
    scrapeDataBuilder.appendHistograms(currentData.metrics.rangeSamplers)
    preparedScrapeData = scrapeDataBuilder.build()
  }

  def scrapeData(): String = preparedScrapeData
}

/**
  * TOOD the whole configuration part is probably not needed, but keeping as it is so that
  * it does not diverge away from the original
  */
object NewPrometheusReporter {

  case class Configuration(
      startEmbeddedServer: Boolean,
      embeddedServerHostname: String,
      embeddedServerPort: Int,
      defaultBuckets: Seq[java.lang.Double],
      timeBuckets: Seq[java.lang.Double],
      informationBuckets: Seq[java.lang.Double],
      customBuckets: Map[String, Seq[java.lang.Double]],
      includeEnvironmentTags: Boolean
  )

  object Configuration {

    def readConfiguration(config: Config): NewPrometheusReporter.Configuration = {
      val prometheusConfig = config.getConfig("kamon.prometheus")

      NewPrometheusReporter.Configuration(
        startEmbeddedServer = prometheusConfig.getBoolean("start-embedded-http-server"),
        embeddedServerHostname = prometheusConfig.getString("embedded-server.hostname"),
        embeddedServerPort = prometheusConfig.getInt("embedded-server.port"),
        defaultBuckets = prometheusConfig.getDoubleList("buckets.default-buckets").asScala,
        timeBuckets = prometheusConfig.getDoubleList("buckets.time-buckets").asScala,
        informationBuckets = prometheusConfig.getDoubleList("buckets.information-buckets").asScala,
        customBuckets = readCustomBuckets(prometheusConfig.getConfig("buckets.custom")),
        includeEnvironmentTags = prometheusConfig.getBoolean("include-environment-tags")
      )
    }

    def environmentTags(reporterConfiguration: NewPrometheusReporter.Configuration) =
      if (reporterConfiguration.includeEnvironmentTags) Kamon.environment.tags
      else Map.empty[String, String]

    private def readCustomBuckets(customBuckets: Config): Map[String, Seq[java.lang.Double]] =
      customBuckets.topLevelKeys
        .map(k => (k, customBuckets.getDoubleList(ConfigUtil.quoteString(k)).asScala))
        .toMap
  }

  def service(reporter: NewPrometheusReporter) = HttpRoutes.of[Task] {
    case GET -> Root => Ok(reporter.scrapeData())
  }
}
