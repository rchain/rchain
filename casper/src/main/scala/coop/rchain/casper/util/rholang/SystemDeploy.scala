package coop.rchain.casper.util.rholang

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType.Extractor
import shapeless.Witness
import coop.rchain.crypto.PublicKey

abstract class SystemDeploy(initialRand: Blake2b512Random) {

  import SystemDeployPlatformFailure._
  import coop.rchain.models._
  import GUnforgeable.UnfInstance.GPrivateBody
  import rholang.{implicits => toPar}
  import shapeless.syntax.singleton._

  type Output
  type Result
  type Env

  final val rand = initialRand.copy()

  final val `sys:casper:return`     = Witness("sys:casper:return")
  final val `sys:casper:deployerId` = Witness("sys:casper:deployerId")
  type `sys:casper:deployerId` = `sys:casper:deployerId`.T
  type `sys:casper:return`     = `sys:casper:return`.T

  protected def toEnvMap: ToEnvMap[Env]
  final def env: Map[String, Par] = normalizerEnv.toEnv(toEnvMap)

  implicit protected val envsReturnChannel: Contains.Aux[Env, `sys:casper:return`, GUnforgeable]
  protected def mkReturnChannel =
    "sys:casper:return" ->> GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(rand.next()))))
  final def returnChannel: Par = toPar(normalizerEnv.get[`sys:casper:return`])

  protected def mkDeployerId(pk: PublicKey) =
    "sys:casper:deployerId" ->> GDeployerId(ByteString.copyFrom(pk.bytes))

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
