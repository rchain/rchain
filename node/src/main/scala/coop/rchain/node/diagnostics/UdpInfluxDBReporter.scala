package coop.rchain.node.diagnostics

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel
import coop.rchain.node.diagnostics.UdpInfluxDBReporter.{MetricDataPacketBuffer, Settings}
import com.typesafe.config.Config
import kamon.Kamon
import kamon.module.MetricReporter
import kamon.metric._
import kamon.status.Environment
import kamon.tag.{Tag, TagSet}
import kamon.util.EnvironmentTags

import java.time.Instant
import java.util.concurrent.TimeUnit

// TODO use Dispatcher to execute inside F context?
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UdpInfluxDBReporter(config: Config = Kamon.config()) extends MetricReporter {

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var settings: Settings             = readConfiguration(config)
  private val clientChannel: DatagramChannel = DatagramChannel.open()

  override def stop(): Unit = {}

  override def reconfigure(config: Config): Unit =
    settings = readConfiguration(config)

  protected def getTimestamp(instant: Instant): String =
    settings.measurementPrecision match {
      case "s" =>
        instant.getEpochSecond.toString
      case "ms" =>
        instant.toEpochMilli.toString
      case "u" | "µ" =>
        ((BigInt(instant.getEpochSecond) * 1000000) + TimeUnit.NANOSECONDS.toMicros(
          instant.getNano.toLong
        )).toString
      case "ns" =>
        ((BigInt(instant.getEpochSecond) * 1000000000) + instant.getNano).toString
    }

  private def readConfiguration(config: Config): Settings = {
    import scala.jdk.CollectionConverters._
    val influxConfig = config.getConfig("kamon.influxdb")
    val address =
      new InetSocketAddress(influxConfig.getString("hostname"), influxConfig.getInt("port"))
    val maxPacketSize = influxConfig.getBytes("max-packet-size")
    val percentiles   = influxConfig.getDoubleList("percentiles").asScala.map(_.toDouble).toSeq
    val precision     = influxConfig.getString("precision")
    val additionalTags =
      EnvironmentTags.from(Environment.from(config), config.getConfig("additional-tags"))

    Settings(address, maxPacketSize, percentiles, additionalTags, precision)
  }

  def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    import snapshot._
    val packetBuffer =
      new MetricDataPacketBuffer(settings.maxPacketSize, clientChannel, settings.address)
    val builder   = new StringBuilder
    val timestamp = getTimestamp(snapshot.to)

    counters.foreach { c =>
      writeLongMetricValue(builder, c, "count", timestamp)
      packetBuffer.appendMeasurement(builder.toString)
      builder.clear()
    }

    gauges.foreach { g =>
      writeDoubleMetricValue(builder, g, "value", timestamp)
      packetBuffer.appendMeasurement(builder.toString)
      builder.clear()
    }

    histograms.foreach { h =>
      writeMetricDistribution(builder, h, settings.percentiles, timestamp)
      packetBuffer.appendMeasurement(builder.toString)
      builder.clear()
    }

    rangeSamplers.foreach { rs =>
      writeMetricDistribution(builder, rs, settings.percentiles, timestamp)
      packetBuffer.appendMeasurement(builder.toString)
      builder.clear()
    }
  }

  private def writeLongMetricValue(
      builder: StringBuilder,
      metric: MetricSnapshot.Values[Long],
      fieldName: String,
      timestamp: String
  ): Unit =
    metric.instruments.foreach { instrument =>
      writeNameAndTags(builder, metric.name, instrument.tags)
      writeIntField(builder, fieldName, instrument.value, appendSeparator = false)
      writeTimestamp(builder, timestamp)
    }

  private def writeDoubleMetricValue(
      builder: StringBuilder,
      metric: MetricSnapshot.Values[Double],
      fieldName: String,
      timestamp: String
  ): Unit =
    metric.instruments.foreach { instrument =>
      writeNameAndTags(builder, metric.name, instrument.tags)
      writeDoubleField(builder, fieldName, instrument.value, appendSeparator = false)
      writeTimestamp(builder, timestamp)
    }

  private def writeMetricDistribution(
      builder: StringBuilder,
      metric: MetricSnapshot.Distributions,
      percentiles: Seq[Double],
      timestamp: String
  ): Unit =
    metric.instruments.foreach { instrument =>
      if (instrument.value.count > 0) {
        writeNameAndTags(builder, metric.name, instrument.tags)
        writeIntField(builder, "count", instrument.value.count)
        writeIntField(builder, "sum", instrument.value.sum)
        writeIntField(builder, "mean", instrument.value.sum / instrument.value.count)
        writeIntField(builder, "min", instrument.value.min)

        percentiles.foreach { p =>
          writeDoubleField(
            builder,
            "p" + String.valueOf(p),
            instrument.value.percentile(p).value.toDouble
          )
        }

        writeIntField(builder, "max", instrument.value.max, appendSeparator = false)
        writeTimestamp(builder, timestamp)
      }
    }

  private def writeNameAndTags(builder: StringBuilder, name: String, metricTags: TagSet): Unit = {
    builder
      .append(escapeName(name))

    val tags = (if (settings.additionalTags.nonEmpty()) metricTags.withTags(settings.additionalTags)
                else metricTags).all()

    if (tags.nonEmpty) {
      tags.foreach { t =>
        builder
          .append(',')
          .append(escapeString(t.key))
          .append("=")
          .append(escapeString(Tag.unwrapValue(t).toString))
      }
    }

    builder.append(' ')
  }

  private def escapeName(in: String): String =
    in.replace(" ", "\\ ")
      .replace(",", "\\,")

  private def escapeString(in: String): String =
    in.replace(" ", "\\ ")
      .replace("=", "\\=")
      .replace(",", "\\,")

  def writeDoubleField(
      builder: StringBuilder,
      fieldName: String,
      value: Double,
      appendSeparator: Boolean = true
  ): Unit = {
    builder
      .append(fieldName)
      .append('=')
      .append(String.valueOf(value))

    if (appendSeparator)
      builder.append(',')
  }

  def writeIntField(
      builder: StringBuilder,
      fieldName: String,
      value: Long,
      appendSeparator: Boolean = true
  ): Unit = {
    builder
      .append(fieldName)
      .append('=')
      .append(String.valueOf(value))
      .append('i')

    if (appendSeparator)
      builder.append(',')
  }

  def writeTimestamp(builder: StringBuilder, timestamp: String): Unit =
    builder
      .append(' ')
      .append(timestamp)
      .append("\n")
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object UdpInfluxDBReporter {

  private final class MetricDataPacketBuffer(
      maxPacketSizeInBytes: Long,
      channel: DatagramChannel,
      remote: InetSocketAddress
  ) {
    private val metricSeparator = "\n"
    private val buffer          = new StringBuilder

    def appendMeasurement(measurementData: String): Unit =
      if (fitsOnBuffer(metricSeparator + measurementData)) {
        val mSeparator = if (buffer.nonEmpty) metricSeparator else ""
        buffer.append(mSeparator).append(measurementData)
      } else {
        flush()
        buffer.append(measurementData)
      }

    private def fitsOnBuffer(data: String): Boolean =
      (buffer.length + data.length) <= maxPacketSizeInBytes

    def flush(): Unit = {
      flushToUDP(buffer.toString)
      buffer.clear()
    }

    private def flushToUDP(data: String): Unit =
      channel.send(ByteBuffer.wrap(data.getBytes), remote)
  }

  final case class Settings(
      address: InetSocketAddress,
      maxPacketSize: Long,
      percentiles: Seq[Double],
      additionalTags: TagSet,
      measurementPrecision: String
  )
}
