package coop.rchain.node.diagnostics

import java.lang.StringBuilder
import java.text.{DecimalFormat, DecimalFormatSymbols}
import java.util.Locale
import kamon.status.Environment
import kamon.metric.Metric
import kamon.metric.MetricSnapshot
import kamon.metric.{Counter, Distribution, Gauge}
import kamon.metric.MeasurementUnit
import kamon.metric.MeasurementUnit.{information, none, time}
import kamon.metric.MeasurementUnit.Dimension._

@SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements"))
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

  //Adds counter snapshots to the scrape data
  def appendCounters(
      counters: Seq[MetricSnapshot.Values[Long]]
  ): ScrapeDataBuilder = {
    counters.groupBy(_.name).foreach(appendValueMetric("counter", alwaysIncreasing = true))
    this
  }

  //Adds gauge snapshots to the scrape data
  def appendGauges(
      gauges: Seq[MetricSnapshot.Values[Double]]
  ): ScrapeDataBuilder = {
    gauges.groupBy(_.name).foreach(appendValueMetric("gauge", alwaysIncreasing = false))
    this
  }

  //Adds histogram snapshots to the scrape data
  def appendHistograms(
      histograms: Seq[MetricSnapshot.Distributions]
  ): ScrapeDataBuilder = {
    histograms.groupBy(_.name).foreach(appendDistributionMetric)
    this
  }

  //Adds value snapshots to the scrape data
  private def appendValueMetric(metricType: String, alwaysIncreasing: Boolean)(
      group: (String, Seq[MetricSnapshot.Values[_]])
  ): Unit = {
    val (metricName, snapshots) = group
    val unit                    = snapshots.headOption.map(_.settings.unit).getOrElse(none)
    val normalizedMetricName = normalizeMetricName(metricName, unit) + {
      if (alwaysIncreasing) "_total" else ""
    }

    append("# TYPE ").append(normalizedMetricName).append(" ").append(metricType).append("\n")

    snapshots.foreach(valueSnapshot => {
      valueSnapshot.instruments.foreach(valueInstrument => {
        append(normalizedMetricName)
        appendTags(TagSetToMap.tagSetToMap(valueInstrument.tags))
        append(" ")
        append(format(scale(valueInstrument.value.asInstanceOf[Long], valueSnapshot.settings.unit)))
        append("\n")
      })
    })
  }

  //Adds Distribution Snapshots to the scrape data
  private def appendDistributionMetric(group: (String, Seq[MetricSnapshot.Distributions])): Unit = {
    val (metricName, snapshots) = group
    val unit                    = snapshots.headOption.map(_.settings.unit).getOrElse(none)
    val normalizedMetricName    = normalizeMetricName(metricName, unit)

    append("# TYPE ").append(normalizedMetricName).append(" histogram").append("\n")

    snapshots.foreach(snapshot => {
      val bucketConfig = resolveBucketConfiguration(snapshot)
      val unit         = snapshot.settings.unit
      snapshot.instruments.foreach(distribution => {
        val tags = TagSetToMap.tagSetToMap(distribution.tags)
        if (distribution.value.count > 0) {
          appendHistogramBuckets(
            normalizedMetricName,
            tags,
            distribution,
            bucketConfig,
            unit
          )
        }

        val count = format(distribution.value.count.toDouble)
        val sum   = format(scale(distribution.value.sum, unit))
        appendTimeSerieValue(normalizedMetricName, tags, count, "_count")
        appendTimeSerieValue(normalizedMetricName, tags, sum, "_sum")
      })
    })
  }

  private def appendTimeSerieValue(
      name: String,
      tags: Map[String, String],
      value: String,
      suffix: String
  ): Unit = {
    append(name)
    append(suffix)
    appendTags(tags)
    append(" ")
    append(value)
    append("\n")
  }

  private def resolveBucketConfiguration(
      metric: MetricSnapshot.Distributions
  ): Seq[java.lang.Double] =
    prometheusConfig.customBuckets.getOrElse(
      metric.name,
      metric.settings.unit.dimension match {
        case Time        => prometheusConfig.timeBuckets
        case Information => prometheusConfig.informationBuckets
        case _           => prometheusConfig.defaultBuckets
      }
    )

  private def appendHistogramBuckets(
      name: String,
      tags: Map[String, String],
      distribution: kamon.metric.Instrument.Snapshot[Distribution],
      buckets: Seq[java.lang.Double],
      unit: MeasurementUnit
  ): Unit = {
    val distributionBuckets            = distribution.value.bucketsIterator
    var currentDistributionBucket      = distributionBuckets.next()
    var currentDistributionBucketValue = scale(currentDistributionBucket.value, unit)
    var inBucketCount                  = 0L
    var leftOver                       = currentDistributionBucket.frequency

    buckets.foreach { configuredBucket =>
      val bucketTags = tags + ("le" -> String.valueOf(configuredBucket))

      if (currentDistributionBucketValue <= configuredBucket) {
        inBucketCount += leftOver
        leftOver = 0

        while (distributionBuckets.hasNext && currentDistributionBucketValue <= configuredBucket) {
          currentDistributionBucket = distributionBuckets.next()
          currentDistributionBucketValue = scale(currentDistributionBucket.value, unit)

          if (currentDistributionBucketValue <= configuredBucket) {
            inBucketCount += currentDistributionBucket.frequency
          } else
            leftOver = currentDistributionBucket.frequency
        }
      }

      appendTimeSerieValue(name, bucketTags, format(inBucketCount.toDouble), "_bucket")
    }

    while (distributionBuckets.hasNext) {
      leftOver += distributionBuckets.next().frequency
    }

    appendTimeSerieValue(
      name,
      tags + ("le" -> "+Inf"),
      format(leftOver + inBucketCount.toDouble),
      "_bucket"
    )
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

  //In newer versions of Kamon, scale is replaced by convert which must accept a double.
  private def scale(value: Long, unit: MeasurementUnit): Double = unit.dimension match {
    case Time if unit.magnitude != time.seconds.magnitude =>
      MeasurementUnit.convert(value.toDouble, unit, time.seconds)
    case Information if unit.magnitude != information.bytes.magnitude =>
      MeasurementUnit.convert(value.toDouble, unit, information.bytes)
    case _ => value.toDouble
  }

}
