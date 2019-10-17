package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.RhoType.Extractor
import coop.rchain.models.NormalizerEnv
import shapeless.Witness

abstract class SystemDeploy {
  import SystemDeployFailure._
  import NormalizerEnv._

  type Output
  type Result
  type Env

  final val `sys:casper:initialDeployerId` = Witness("sys:casper:initialDeployerId")
  final val `sys:casper:return`            = Witness("sys:casper:return")

  type `sys:casper:initialDeployerId` = `sys:casper:initialDeployerId`.T
  type `sys:casper:return`            = `sys:casper:return`.T

  def source: String

  protected val extractor: Extractor[Output]
  final def extractResult(output: Par): Either[SystemDeployFailure, Result] =
    extractor
      .unapply(output)
      .fold[Either[SystemDeployFailure, Result]](Left(UnexpectedResult(List(output))))(
        processResult
      )
  protected def processResult(value: extractor.ScalaType): Either[SystemDeployFailure, Result]

  final def env[Env0](
      normalizerEnv: NormalizerEnv[Env0]
  )(implicit ev1: Provides[Env0, Env], ev2: ToEnvMap[Env]): Map[String, Par] =
    normalizerEnv.provide[Env].toEnv

  final def returnChannel[Env0](normalizerEnv: NormalizerEnv[Env0])(
      implicit ev: Provides[Env0, Env]
  ): Par = getReturnChannel(normalizerEnv.provide[Env].env)
  protected def getReturnChannel(env: Env): Par
}

