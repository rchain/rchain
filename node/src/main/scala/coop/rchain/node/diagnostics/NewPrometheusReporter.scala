package coop.rchain.node.diagnostics

import com.typesafe.config.{Config, ConfigUtil}
import kamon._
import kamon.metric._
import kamon.module.MetricReporter
import kamon.prometheus.PrometheusSettings.{GaugeSettings, SummarySettings}
import kamon.tag.TagSet
import kamon.util.Filter.Glob

import java.time.Duration
import scala.jdk.CollectionConverters._

/**
  * Based on kamon-prometheus but without the embedded server
  */
@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements"))
class NewPrometheusReporter extends MetricReporter {
  import NewPrometheusReporter.Configuration.{environmentTags, readConfiguration}

  private val snapshotAccumulator =
    new PeriodSnapshot.Accumulator(Duration.ofDays(365 * 5), Duration.ZERO, Duration.ZERO)

  @volatile private var preparedScrapeData: String =
    "# The kamon-prometheus module didn't receive any data just yet.\n"

  override def stop(): Unit                         = {}
  override def reconfigure(newConfig: Config): Unit = {}

  override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    snapshotAccumulator.add(snapshot)
    val currentData           = snapshotAccumulator.peek()
    val reporterConfiguration = readConfiguration(Kamon.config())
    val scrapeDataBuilder =
      new ScrapeDataBuilder(reporterConfiguration, environmentTags(reporterConfiguration))

    scrapeDataBuilder.appendCounters(currentData.counters)
    scrapeDataBuilder.appendGauges(currentData.gauges)
    scrapeDataBuilder.appendHistograms(currentData.histograms)
    scrapeDataBuilder.appendHistograms(currentData.rangeSamplers)
    preparedScrapeData = scrapeDataBuilder.build()
  }

  def scrapeData(): String = preparedScrapeData
}

/**
  * TOOD the whole configuration part is probably not needed, but keeping as it is so that
  * it does not diverge away from the original
  */
object NewPrometheusReporter {

  final case class Configuration(
      startEmbeddedServer: Boolean,
      embeddedServerHostname: String,
      embeddedServerPort: Int,
      defaultBuckets: Seq[java.lang.Double],
      timeBuckets: Seq[java.lang.Double],
      informationBuckets: Seq[java.lang.Double],
      customBuckets: Map[String, Seq[java.lang.Double]],
      includeEnvironmentTags: Boolean,
      summarySettings: SummarySettings,
      gaugeSettings: GaugeSettings
  )

  object Configuration {

    def readConfiguration(config: Config): NewPrometheusReporter.Configuration = {
      val prometheusConfig = config.getConfig("kamon.prometheus")

      NewPrometheusReporter.Configuration(
        startEmbeddedServer = prometheusConfig.getBoolean("start-embedded-http-server"),
        embeddedServerHostname = prometheusConfig.getString("embedded-server.hostname"),
        embeddedServerPort = prometheusConfig.getInt("embedded-server.port"),
        defaultBuckets = prometheusConfig.getDoubleList("buckets.default-buckets").asScala.toSeq,
        timeBuckets = prometheusConfig.getDoubleList("buckets.time-buckets").asScala.toSeq,
        informationBuckets =
          prometheusConfig.getDoubleList("buckets.information-buckets").asScala.toSeq,
        customBuckets = readCustomBuckets(prometheusConfig.getConfig("buckets.custom")),
        includeEnvironmentTags = prometheusConfig.getBoolean("include-environment-tags"),
        summarySettings = SummarySettings(
          quantiles = prometheusConfig.getDoubleList("summaries.quantiles").asScala.toSeq,
          metricMatchers =
            prometheusConfig.getStringList("summaries.metrics").asScala.map(Glob).toSeq
        ),
        gaugeSettings = GaugeSettings(
          metricMatchers = prometheusConfig.getStringList("gauges.metrics").asScala.map(Glob).toSeq
        )
      )
    }

    def environmentTags(
        reporterConfiguration: NewPrometheusReporter.Configuration
    ): TagSet =
      if (reporterConfiguration.includeEnvironmentTags) Kamon.environment.tags
      else TagSet.Empty

    private def readCustomBuckets(customBuckets: Config): Map[String, Seq[java.lang.Double]] =
      customBuckets.topLevelKeys
        .map(k => (k, customBuckets.getDoubleList(ConfigUtil.quoteString(k)).asScala.toSeq))
        .toMap
  }

  import cats.effect.Sync
  import org.http4s.HttpRoutes

  def service[F[_]: Sync](reporter: NewPrometheusReporter): HttpRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root => Ok(Sync[F].delay(reporter.scrapeData()))
    }
  }
}
