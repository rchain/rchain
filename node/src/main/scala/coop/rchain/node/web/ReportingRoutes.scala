package coop.rchain.node.web

import cats.effect.Sync
import cats.implicits._
import cats.{~>, Applicative}
import com.google.protobuf.ByteString
import coop.rchain.casper.{ReportBlockNotFound, ReportError, ReportReplayError}
import coop.rchain.models.BlockHash._
import coop.rchain.casper.ReportingCasper
import coop.rchain.casper.protocol.ProcessedDeploy
import coop.rchain.casper.util.rholang.{InternalError, ReplayFailure}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.rspace.ReportingRspace.ReportingEvent
import coop.rchain.rspace.ReportingTransformer.ReportingRhoStringTransformer._
import org.http4s.{HttpRoutes, QueryParamDecoder}
import coop.rchain.rspace.ReportingTransformer
import coop.rchain.rholang.interpreter.{PrettyPrinter => RhoPrinter}
import org.http4s.circe.jsonEncoderOf
import coop.rchain.models.syntax._

object ReportingRoutes {

  val prettyPrinter = RhoPrinter()
  val transformer =
    new ReportingEventStringTransformer[Par, BindPattern, ListParWithRandom, TaggedContinuation](
      serializeC = channel => prettyPrinter.buildString(channel),
      serializeP = p => p.patterns.map(prettyPrinter.buildString).mkString("[", ";", "]"),
      serializeA = data => data.pars.map(prettyPrinter.buildString).mkString("[", ";", "]"),
      serializeK = k =>
        k.taggedCont match {
          case ParBody(value)      => prettyPrinter.buildString(value.body)
          case ScalaBodyRef(value) => s"ScalaBodyRef($value)"
          case Empty               => "empty"
        }
    )

  final case class DeployTrace(deployHash: String, source: String, events: List[List[RhoEvent]])
  sealed trait Report
  final case class BlockTracesReport(
      hash: String,
      traces: List[DeployTrace]
  ) extends Report
  final case class BlockNotFound(hash: String)    extends Report
  final case class BlockReplayError(hash: String) extends Report
  final case class BlockReplayFailed(hash: String, msg: String, deployId: Option[String])
      extends Report

  def transformDeploy[C, P, A, K](transformer: ReportingTransformer[C, P, A, K, RhoEvent])(
      ipd: ProcessedDeploy,
      events: Seq[Seq[ReportingEvent]]
  ): DeployTrace =
    DeployTrace(
      ipd.deploy.sig.base16String,
      ipd.deploy.data.term,
      events.map(a => a.map(transformer.transformEvent).toList).toList
    )

  def transforResult[F[_]: Sync](
      hash: BlockHash,
      state: F[
        Either[ReportError, List[(ProcessedDeploy, Seq[Seq[ReportingEvent]])]]
      ]
  ): F[Report] =
    state.map(
      s =>
        s match {
          case Left(ReportBlockNotFound(hash)) => BlockNotFound(hash.base16String)
          case Left(ReportReplayError(error)) =>
            BlockReplayFailed(
              hash.base16String,
              error.toString,
              None
            )
          case Right(deploys) =>
            val t = transformDeploy(transformer)(_, _)
            BlockTracesReport(
              hash.base16String,
              deploys.map(Function.tupled(t))
            )
          case _ => BlockReplayError(hash.base16String)
        }
    )

  def service[F[_]: Sync, M[_]: Applicative](
      reportingCasper: ReportingCasper[M]
  )(implicit nt: M ~> F): HttpRoutes[F] = {
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.auto._

    implicit val genDevConfig: Configuration =
      Configuration.default
        .withDiscriminator("type")
        .withKebabCaseConstructorNames
        .withKebabCaseMemberNames

    implicit val BlockHashQueryParamDecoder: QueryParamDecoder[BlockHash] =
      QueryParamDecoder[String].map(s => ByteString.copyFrom(Base16.decode(s).get))

    object BlockHashQueryParamMatcher extends QueryParamDecoderMatcher[ByteString]("blockHash")

    implicit val prepareEncoder = jsonEncoderOf[F, Report]

    HttpRoutes.of[F] {
      case GET -> Root / "trace" :? BlockHashQueryParamMatcher(hash) =>
        Ok { transforResult(hash, nt(reportingCasper.trace(hash))) }
    }
  }
}
