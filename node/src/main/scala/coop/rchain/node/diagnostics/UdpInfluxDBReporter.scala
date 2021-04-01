package coop.rchain.node.diagnostics

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.DatagramChannel

import coop.rchain.node.diagnostics.UdpInfluxDBReporter.{MetricDataPacketBuffer, Settings}

import com.typesafe.config.Config
import kamon.Kamon
import kamon.module.MetricReporter
import kamon.metric._
import kamon.util.EnvironmentTags

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UdpInfluxDBReporter(config: Config = Kamon.config()) extends MetricReporter {

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var settings: Settings             = readConfiguration(config)
  private val clientChannel: DatagramChannel = DatagramChannel.open()

  override def stop(): Unit = {}

  override def reconfigure(config: Config): Unit =
    settings = readConfiguration(config)

  private def readConfiguration(config: Config): Settings = {
    import scala.collection.JavaConverters._
    val influxConfig = config.getConfig("kamon.influxdb")
    val address =
      new InetSocketAddress(influxConfig.getString("hostname"), influxConfig.getInt("port"))
    val maxPacketSize = influxConfig.getBytes("max-packet-size")
    val percentiles   = influxConfig.getDoubleList("percentiles").asScala.map(_.toDouble)
    val additionalTags = TagSetToMap.tagSetToMap(
      EnvironmentTags.from(Kamon.environment, influxConfig.getConfig("additional-tags"))
    )

    Settings(address, maxPacketSize, percentiles, additionalTags)
  }

  def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit = {
    import snapshot._
    val packetBuffer =
      new MetricDataPacketBuffer(settings.maxPacketSize, clientChannel, settings.address)
    val builder   = StringBuilder.newBuilder
    val timestamp = snapshot.to.toEpochMilli

    counters.foreach(counterSnapshot => {
      val name = counterSnapshot.name
      counterSnapshot.instruments.foreach(counterInstrument => {
        writeMetricValue(builder, counterInstrument, "count", timestamp, name)
        packetBuffer.appendMeasurement(builder.toString)
        builder.clear()
      })
    })
    gauges.foreach(gaugeSnapshot => {
      val name = gaugeSnapshot.name
      gaugeSnapshot.instruments.foreach(gaugeInstrument => {
        writeMetricValue(builder, gaugeInstrument, "value", timestamp, name)
        packetBuffer.appendMeasurement(builder.toString)
        builder.clear()
      })
    })
    histograms.foreach(histogramSnapshot => {
      val name = histogramSnapshot.name
      histogramSnapshot.instruments.foreach(histogramInstrument => {
        writeMetricDistribution(builder, histogramInstrument, settings.percentiles, timestamp, name)
        packetBuffer.appendMeasurement(builder.toString)
        builder.clear()
      })
    })
    rangeSamplers.foreach(rangeSamplerSnapshot => {
      val name = rangeSamplerSnapshot.name
      rangeSamplerSnapshot.instruments.foreach(rangeSamplerInstrument => {
        writeMetricDistribution(
          builder,
          rangeSamplerInstrument,
          settings.percentiles,
          timestamp,
          name
        )
        packetBuffer.appendMeasurement(builder.toString)
        builder.clear()
      })
    })

  }

  private def writeMetricValue(
      builder: StringBuilder,
      metric: Instrument.Snapshot[_],
      fieldName: String,
      timestamp: Long,
      name: String
  ): Unit = {
    writeNameAndTags(builder, name, TagSetToMap.tagSetToMap(metric.tags))
    writeIntField(builder, fieldName, metric.value.asInstanceOf[Long], appendSeparator = false)
    writeTimestamp(builder, timestamp)
  }

  private def writeMetricDistribution(
      builder: StringBuilder,
      metric: Instrument.Snapshot[Distribution],
      percentiles: Seq[Double],
      timestamp: Long,
      name: String
  ): Unit = {
    writeNameAndTags(builder, name, TagSetToMap.tagSetToMap(metric.tags))
    writeIntField(builder, "count", metric.value.count)
    writeIntField(builder, "sum", metric.value.sum)
    writeIntField(builder, "min", metric.value.min)

    percentiles.foreach(p => {
      writeDoubleField(
        builder,
        "p" + String.valueOf(p),
        metric.value.percentile(p).value.toDouble
      )
    })

    writeIntField(builder, "max", metric.value.max, appendSeparator = false)
    writeTimestamp(builder, timestamp)
  }

  private def writeNameAndTags(
      builder: StringBuilder,
      name: String,
      metricTags: Map[String, String]
  ): Unit = {
    builder.append(name)

    val tags =
      if (settings.additionalTags.nonEmpty) metricTags ++ settings.additionalTags
      else metricTags

    if (tags.nonEmpty) {
      tags.foreach {
        case (key, value) =>
          builder
            .append(',')
            .append(escapeString(key))
            .append("=")
            .append(escapeString(value))
      }
    }

    builder.append(' ')
  }

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

  def writeTimestamp(builder: StringBuilder, timestamp: Long): Unit =
    builder
      .append(' ')
      .append(timestamp)
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object UdpInfluxDBReporter {

  private final class MetricDataPacketBuffer(
      maxPacketSizeInBytes: Long,
      channel: DatagramChannel,
      remote: InetSocketAddress
  ) {
    private val metricSeparator = "\n"
    private val buffer          = StringBuilder.newBuilder

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
      additionalTags: Map[String, String]
  )
}
