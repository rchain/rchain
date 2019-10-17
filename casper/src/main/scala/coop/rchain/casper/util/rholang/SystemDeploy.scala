package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.RhoType.Extractor

abstract class SystemDeploy(val id: String) {
  import FailedSystemDeploy._
  type Output
  type Result

  def code: String

  protected val extractor: Extractor[Output]
  def consume(output: Par): Either[FailedSystemDeploy, Result] =
    extractor
      .unapply(output)
      .fold[Either[FailedSystemDeploy, Result]](Left(ConsumeError(id, output)))(result)
  protected def result(value: extractor.ScalaType): Either[FailedSystemDeploy, Result]

  protected final def deployError(message: String) = FailedSystemDeploy.DeployError(id, message)
}
