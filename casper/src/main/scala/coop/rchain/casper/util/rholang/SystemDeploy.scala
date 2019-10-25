package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedSystemDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType.Extractor
import shapeless.Witness

abstract class SystemDeploy(val rand: Blake2b512Random) {

  import SystemDeployPlatformFailure._
  import coop.rchain.models._
  import GUnforgeable.UnfInstance.GPrivateBody
  import rholang.{implicits => toPar}
  import shapeless.syntax.singleton._

  type Output
  type Result
  type Env

  final val `sys:casper:return` = Witness("sys:casper:return")
  type `sys:casper:return` = `sys:casper:return`.T

  protected def toEnvMap: ToEnvMap[Env]
  final def env: Map[String, Par] = normalizerEnv.toEnv(toEnvMap)

  implicit protected val envsReturnChannel: Contains.Aux[Env, `sys:casper:return`, GUnforgeable]
  protected def mkReturnChannel =
    "sys:casper:return" ->> GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(rand.next()))))
  final def returnChannel: Par = toPar(normalizerEnv.get[`sys:casper:return`])

  protected val normalizerEnv: NormalizerEnv[Env]

  protected val extractor: Extractor[Output]

  val source: String

  final def extractResult(output: Par): Either[SystemDeployFailure, Result] =
    extractor
      .unapply(output)
      .fold[Either[SystemDeployFailure, Result]](Left(UnexpectedResult(List(output))))(
        processResult
      )

  protected def processResult(value: extractor.ScalaType): Either[SystemDeployFailure, Result]
}

sealed abstract class SystemDeployResult[+A](
    val processedSystemDeploy: ProcessedSystemDeploy,
    val deploySpecificResult: Option[A]
)
object SystemDeployResult {
  import coop.rchain.rspace.trace.Log

  final case class Succeeded[A](processed: ProcessedSystemDeploy.Succeeded, result: A)
      extends SystemDeployResult[A](processed, Some(result))
  final case class Failed[A](processed: ProcessedSystemDeploy.Failed)
      extends SystemDeployResult[A](processed, None)

  def succeeded[A](log: Log, result: A) =
    Succeeded(
      ProcessedSystemDeploy
        .Succeeded(log.map(EventConverter.toCasperEvent).toList),
      result
    )

  def failed[A](log: Log, errorMsg: String) =
    Failed(
      ProcessedSystemDeploy
        .Failed(log.map(EventConverter.toCasperEvent).toList, errorMsg)
    )
}
