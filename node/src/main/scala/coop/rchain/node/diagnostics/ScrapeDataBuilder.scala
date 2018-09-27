package coop.rchain.node.diagnostics

import java.lang.StringBuilder
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale

import kamon.Environment
import kamon.metric.{MetricDistribution, MetricValue}
import kamon.metric.MeasurementUnit
import kamon.metric.MeasurementUnit.{information, none, time}
import kamon.metric.MeasurementUnit.Dimension._

class ScrapeDataBuilder(
    prometheusConfig: NewPrometheusReporter.Configuration,
    environmentTags: Map[String, String] = Map.empty
) {
  private val builder              = new StringBuilder()
  private val decimalFormatSymbols = DecimalFormatSymbols.getInstance(Locale.ROOT)
  private val numberFormat         = new DecimalFormat("#0.0########", decimalFormatSymbols)

  import builder.append

  def build(): String =
    builder.toString()

  def appendCounters(counters: Seq[MetricValue]): ScrapeDataBuilder = {
    counters.groupBy(_.name).foreach(appendValueMetric("counter", alwaysIncreasing = true))
    this
  }

  def appendGauges(gauges: Seq[MetricValue]): ScrapeDataBuilder = {
    gauges.groupBy(_.name).foreach(appendValueMetric("gauge", alwaysIncreasing = false))
    this
  }

  def appendHistograms(histograms: Seq[MetricDistribution]): ScrapeDataBuilder = {
    histograms.groupBy(_.name).foreach(appendDistributionMetric)
    this
  }

  private def appendValueMetric(metricType: String, alwaysIncreasing: Boolean)(
      group: (String, Seq[MetricValue])
  ): Unit = {
    val (metricName, snapshots) = group
    val unit                    = snapshots.headOption.map(_.unit).getOrElse(none)
    val normalizedMetricName = normalizeMetricName(metricName, unit) + {
      if (alwaysIncreasing) "_total" else ""
    }

    append("# TYPE ").append(normalizedMetricName).append(" ").append(metricType).append("\n")

    snapshots.foreach(metric => {
      append(normalizedMetricName)
      appendTags(metric.tags)
      append(" ")
      append(format(scale(metric.value, metric.unit)))
      append("\n")
    })
  }

  private def appendDistributionMetric(group: (String, Seq[MetricDistribution])): Unit = {
    val (metricName, snapshots) = group
    val unit                    = snapshots.headOption.map(_.unit).getOrElse(none)
    val normalizedMetricName    = normalizeMetricName(metricName, unit)

    append("# TYPE ").append(normalizedMetricName).append(" histogram").append("\n")

    snapshots.foreach(metric => {
      if (metric.distribution.count > 0) {
        appendHistogramBuckets(
          normalizedMetricName,
          metric.tags,
          metric,
          resolveBucketConfiguration(metric)
        )

        val count = format(metric.distribution.count)
        val sum   = format(scale(metric.distribution.sum, metric.unit))
        appendTimeSerieValue(normalizedMetricName, metric.tags, count, "_count")
        appendTimeSerieValue(normalizedMetricName, metric.tags, sum, "_sum")
      }
    })
  }

  private def appendTimeSerieValue(
      name: String,
      tags: Map[String, String],
      value: String,
      suffix: String = ""
  ): Unit = {
    append(name)
    append(suffix)
    appendTags(tags)
    append(" ")
    append(value)
    append("\n")
  }

  private def resolveBucketConfiguration(metric: MetricDistribution): Seq[java.lang.Double] =
    prometheusConfig.customBuckets.getOrElse(
      metric.name,
      metric.unit.dimension match {
        case Time        => prometheusConfig.timeBuckets
        case Information => prometheusConfig.informationBuckets
        case _           => prometheusConfig.defaultBuckets
      }
    )

  private def appendHistogramBuckets(
      name: String,
      tags: Map[String, String],
      metric: MetricDistribution,
      buckets: Seq[java.lang.Double]
  ): Unit = {
    val distributionBuckets            = metric.distribution.bucketsIterator
    var currentDistributionBucket      = distributionBuckets.next()
    var currentDistributionBucketValue = scale(currentDistributionBucket.value, metric.unit)
    var inBucketCount                  = 0L
    var leftOver                       = currentDistributionBucket.frequency

    buckets.foreach { configuredBucket =>
      val bucketTags = tags + ("le" -> String.valueOf(configuredBucket))

      if (currentDistributionBucketValue <= configuredBucket) {
        inBucketCount += leftOver
        leftOver = 0

        while (distributionBuckets.hasNext && currentDistributionBucketValue <= configuredBucket) {
          currentDistributionBucket = distributionBuckets.next()
          currentDistributionBucketValue = scale(currentDistributionBucket.value, metric.unit)

          if (currentDistributionBucketValue <= configuredBucket) {
            inBucketCount += currentDistributionBucket.frequency
          } else
            leftOver = currentDistributionBucket.frequency
        }
      }

      appendTimeSerieValue(name, bucketTags, format(inBucketCount), "_bucket")
    }

    while (distributionBuckets.hasNext) {
      leftOver += distributionBuckets.next().frequency
    }

    appendTimeSerieValue(name, tags + ("le" -> "+Inf"), format(leftOver + inBucketCount), "_bucket")
  }

  private def appendTags(tags: Map[String, String]): Unit = {
    val allTags = tags ++ environmentTags
    if (allTags.nonEmpty) append("{")

    val tagIterator = allTags.iterator
    var tagCount    = 0

    while (tagIterator.hasNext) {
      val (key, value) = tagIterator.next()
      if (tagCount > 0) append(",")
      append(normalizeLabelName(key)).append("=\"").append(value).append('"')
      tagCount += 1
    }

    if (allTags.nonEmpty) append("}")
  }

  private def normalizeMetricName(metricName: String, unit: MeasurementUnit): String = {
    val normalizedMetricName = metricName.map(charOrUnderscore)

    unit.dimension match {
      case Time        => normalizedMetricName + "_seconds"
      case Information => normalizedMetricName + "_bytes"
      case _           => normalizedMetricName
    }
  }

  private def normalizeLabelName(label: String): String =
    label.map(charOrUnderscore)

  private def charOrUnderscore(char: Char): Char =
    if (char.isLetterOrDigit || char == '_') char else '_'

  private def format(value: Double): String =
    numberFormat.format(value)

  private def scale(value: Long, unit: MeasurementUnit): Double = unit.dimension match {
    case Time if unit.magnitude != time.seconds.magnitude =>
      MeasurementUnit.scale(value, unit, time.seconds)
    case Information if unit.magnitude != information.bytes.magnitude =>
      MeasurementUnit.scale(value, unit, information.bytes)
    case _ => value
  }

}
