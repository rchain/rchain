package coop.rchain.node.diagnostics

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.Config
import coop.rchain.node.diagnostics.BatchInfluxDBReporter.Settings
import fs2.concurrent.Channel
import kamon.metric._
import kamon.util.EnvironmentTagBuilder
import kamon.{Kamon, MetricReporter}
import okhttp3._
import org.slf4j.LoggerFactory

import java.io.IOException
import scala.concurrent.duration._
import scala.util.Try

// TODO get rid of monix
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class BatchInfluxDBReporter(config: Config = Kamon.config()) extends MetricReporter {
  private val logger = LoggerFactory.getLogger(classOf[BatchInfluxDBReporter])
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private var settings = readSettings(config)
  private val client   = buildClient(settings)
  private val subject  = Channel.unbounded[IO, Option[Seq[String]]].unsafeRunSync()
  override def start(): Unit =
    subject.stream.unNoneTerminate
      .evalMap(postMetrics)
      .compile
      .drain
      .unsafeRunSync()

  override def stop(): Unit = subject.send(None) // finish stream

  override def reconfigure(config: Config): Unit = {
    stop()
    settings = readSettings(config)
    start()
  }

  private def readSettings(config: Config): Settings = {
    import scala.collection.JavaConverters._
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

    val additionalTags = EnvironmentTagBuilder.create(root.getConfig("additional-tags"))

    Settings(
      url,
      interval,
      root.getDoubleList("percentiles").asScala.map(_.toDouble),
      credentials,
      additionalTags
    )
  }

  override def reportPeriodSnapshot(snapshot: PeriodSnapshot): Unit =
    subject.send(Seq(translateToLineProtocol(snapshot)).some).unsafeRunSync()

  private def postMetrics(metrics: Seq[String]): IO[Unit] =
    IO.async_ {
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
    import periodSnapshot.metrics._
    val builder   = StringBuilder.newBuilder
    val timestamp = periodSnapshot.to.toEpochMilli

    counters.foreach(c => writeMetricValue(builder, c, "count", timestamp))
    gauges.foreach(g => writeMetricValue(builder, g, "value", timestamp))
    histograms.foreach(h => writeMetricDistribution(builder, h, settings.percentiles, timestamp))
    rangeSamplers.foreach(
      rs => writeMetricDistribution(builder, rs, settings.percentiles, timestamp)
    )

    builder.result()
  }

  private def writeMetricValue(
      builder: StringBuilder,
      metric: MetricValue,
      fieldName: String,
      timestamp: Long
  ): Unit = {
    writeNameAndTags(builder, metric.name, metric.tags)
    writeIntField(builder, fieldName, metric.value, appendSeparator = false)
    writeTimestamp(builder, timestamp)
  }

  private def writeMetricDistribution(
      builder: StringBuilder,
      metric: MetricDistribution,
      percentiles: Seq[Double],
      timestamp: Long
  ): Unit = {
    writeNameAndTags(builder, metric.name, metric.tags)
    writeIntField(builder, "count", metric.distribution.count)
    writeIntField(builder, "sum", metric.distribution.sum)
    writeIntField(builder, "min", metric.distribution.min)

    percentiles.foreach(p => {
      writeDoubleField(
        builder,
        "p" + String.valueOf(p),
        metric.distribution.percentile(p).value.toDouble
      )
    })

    writeIntField(builder, "max", metric.distribution.max, appendSeparator = false)
    writeTimestamp(builder, timestamp)
  }

  private def writeNameAndTags(
      builder: StringBuilder,
      name: String,
      metricTags: Map[String, String]
  ): Unit = {
    builder
      .append(name)

    val tags =
      if (settings.additionalTags.nonEmpty) metricTags ++ settings.additionalTags else metricTags

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
      additionalTags: Map[String, String]
  )
}
