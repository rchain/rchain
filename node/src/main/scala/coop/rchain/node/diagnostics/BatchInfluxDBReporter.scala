package coop.rchain.node.diagnostics

import cats.effect.IO
import cats.effect.kernel.Async
import cats.effect.std.Supervisor
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.Config
import coop.rchain.node.diagnostics.BatchInfluxDBReporter.Settings
import fs2.concurrent.Channel
import kamon.metric._
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.module.{MetricReporter, ModuleFactory}
import kamon.status.Environment
import kamon.tag.{Tag, TagSet}
import kamon.util.EnvironmentTags
import okhttp3._
import org.slf4j.LoggerFactory

import java.io.IOException
import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class BatchInfluxDBReporter[F[_]: Async](
    dispatcher: cats.effect.std.Dispatcher[F],
    config: Config = Kamon.config()
) extends MetricReporter {
  private val logger = LoggerFactory.getLogger(classOf[BatchInfluxDBReporter[F]])
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var settings = readSettings(config)
  private val client   = buildClient(settings)
  private val subject  = dispatcher.unsafeRunSync(Channel.unbounded[F, Option[Seq[String]]])

  start()

  override def stop(): Unit = subject.send(None) // finish stream

  override def reconfigure(config: Config): Unit = {
    stop()
    settings = readSettings(config)
    start()
  }

  override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit =
    dispatcher.unsafeRunSync(subject.send(Seq(translateToLineProtocol(snapshot)).some))

  private def start(): Unit = {
    // TODO implement accumulation over time interval settings.batchInterval
    val batching = subject.stream.unNoneTerminate.evalMap(postMetrics).compile
    dispatcher.unsafeRunAndForget(batching.drain)
  }

  private def readSettings(config: Config): Settings = {
    import scala.jdk.CollectionConverters._
    val root       = config.getConfig("kamon.influxdb")
    val host       = root.getString("hostname")
    val authConfig = Try(root.getConfig("authentication")).toOption
    val credentials =
      authConfig.map(conf => Credentials.basic(conf.getString("user"), conf.getString("password")))
    val port     = root.getInt("port")
    val database = root.getString("database")
    val protocol = root.getString("protocol").toLowerCase
    val url      = s"$protocol://$host:$port/write?precision=ms&db=$database"
    val interval =
      if (root.hasPath("batch-interval"))
        Duration.fromNanos(root.getDuration("batch-interval").toNanos)
      else 10.seconds
    val precision = root.getString("precision")

    val additionalTags =
      EnvironmentTags.from(Environment.from(config), root.getConfig("additional-tags"))

    Settings(
      url,
      interval,
      root.getDoubleList("percentiles").asScala.map(_.toDouble).toSeq,
      credentials,
      additionalTags,
      precision
    )
  }

  private def postMetrics(metrics: Seq[String]): F[Unit] =
    Async[F].async_ {
      case cb =>
        val body = RequestBody.create(MediaType.parse("text/plain"), metrics.mkString)
        val request = new Request.Builder()
          .url(settings.url)
          .post(body)
          .build()

        client
          .newCall(request)
          .enqueue(
            new Callback {
              def onFailure(call: Call, e: IOException): Unit = {
                logger.error("Failed to POST metrics to InfluxDB", e)
                cb(Right(()))
              }

              def onResponse(call: Call, response: Response): Unit = {
                if (response.isSuccessful)
                  logger.trace("Successfully sent metrics to InfluxDB")
                else {
                  logger.error(
                    "Metrics POST to InfluxDB failed with status code [{}], response body: {}",
                    response.code(),
                    response.body().string()
                  )
                }
                cb(Right(()))
              }
            }
          )
    }

  private def translateToLineProtocol(periodSnapshot: PeriodSnapshot): String = {
    import periodSnapshot._
    val builder   = new StringBuilder
    val timestamp = getTimestamp(periodSnapshot.to)

    counters.foreach(c => writeLongMetricValue(builder, c, "count", timestamp))
    gauges.foreach(g => writeDoubleMetricValue(builder, g, "value", timestamp))
    histograms.foreach(h => writeMetricDistribution(builder, h, settings.percentiles, timestamp))
    rangeSamplers.foreach(
      rs => writeMetricDistribution(builder, rs, settings.percentiles, timestamp)
    )

    builder.result()
  }

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

  protected def buildClient(settings: Settings): OkHttpClient = {
    val basicBuilder = new OkHttpClient.Builder()
    val authenticator = settings.credentials.map(
      credentials =>
        new Authenticator() {
          def authenticate(route: Route, response: Response): Request =
            response.request().newBuilder().header("Authorization", credentials).build()
        }
    )
    authenticator
      .foldLeft(basicBuilder) { case (builder, auth) => builder.authenticator(auth) }
      .build()
  }
}

object BatchInfluxDBReporter {
  final case class Settings(
      url: String,
      batchInterval: FiniteDuration,
      percentiles: Seq[Double],
      credentials: Option[String],
      additionalTags: TagSet,
      measurementPrecision: String
  )
}
