package coop.rchain.casper.util.rholang

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.RhoType.Extractor
import coop.rchain.models.NormalizerEnv
import coop.rchain.models.NormalizerEnv.ToEnvMap
import shapeless.Witness
import coop.rchain.casper.protocol.ProcessedSystemDeploy
import coop.rchain.casper.util.EventConverter

abstract class SystemDeploy(val rand: Blake2b512Random) {

  import SystemDeployPlatformFailure._

  type Output
  type Result
  type Env

  final val `sys:casper:initialDeployerId` = Witness("sys:casper:initialDeployerId")
  final val `sys:casper:return`            = Witness("sys:casper:return")

  type `sys:casper:initialDeployerId` = `sys:casper:initialDeployerId`.T
  type `sys:casper:return`            = `sys:casper:return`.T

  protected def toEnvMap: ToEnvMap[Env]
  final def env: Map[String, Par] = normalizerEnv.toEnv(toEnvMap)

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

  final def returnChannel: Par = getReturnChannel(normalizerEnv.env)

  protected def getReturnChannel(env: Env): Par

}

sealed abstract class SystemDeployResult[+A](
    val processedSystemDeploy: ProcessedSystemDeploy,
    val deploySpecificResult: Option[A]
)
object SystemDeployResult {
  import coop.rchain.rholang.interpreter.accounting.Cost
  import coop.rchain.rspace.trace.Log

  final case class Succeeded[A](processed: ProcessedSystemDeploy.Succeeded, result: A)
      extends SystemDeployResult[A](processed, Some(result))
  final case class Failed[A](processed: ProcessedSystemDeploy.Failed)
      extends SystemDeployResult[A](processed, None)

  def succeeded[A](cost: Cost, log: Log, result: A) =
    Succeeded(
      ProcessedSystemDeploy
        .Succeeded(Cost.toProto(cost), log.map(EventConverter.toCasperEvent).toList),
      result
    )
  def failed[A](cost: Cost, log: Log, errorMsg: String) =
    Failed(
      ProcessedSystemDeploy
        .Failed(Cost.toProto(cost), log.map(EventConverter.toCasperEvent).toList, errorMsg)
    )
}
