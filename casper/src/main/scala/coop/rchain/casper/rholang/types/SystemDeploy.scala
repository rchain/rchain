package coop.rchain.casper.rholang.types

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.rholang.types.SystemDeployPlatformFailure.UnexpectedResult
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.NormalizerEnv.{Contains, ToEnvMap}
import coop.rchain.rholang.interpreter.RhoType.Extractor
import shapeless.Witness

abstract class SystemDeploy(initialRand: Blake2b512Random) {

  import coop.rchain.models._
  import GUnforgeable.UnfInstance.GPrivateBody
  import rholang.{implicits => toPar}
  import shapeless.syntax.singleton._

  type Output
  type Result
  type Env

  final val rand = initialRand.copy()

  final val `sys:casper:deployerId` = Witness("sys:casper:deployerId")
  final val `sys:casper:authToken`  = Witness("sys:casper:authToken")
  final val `sys:casper:return`     = Witness("sys:casper:return")
  type `sys:casper:deployerId` = `sys:casper:deployerId`.T
  type `sys:casper:authToken`  = `sys:casper:authToken`.T
  type `sys:casper:return`     = `sys:casper:return`.T

  protected def toEnvMap: ToEnvMap[Env]
  final def env: Map[String, Par] = normalizerEnv.toEnv(toEnvMap)

  implicit protected val envsReturnChannel: Contains.Aux[Env, `sys:casper:return`, GUnforgeable]
  protected def mkReturnChannel =
    "sys:casper:return" ->> GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(rand.next()))))
  final def returnChannel: Par = toPar(normalizerEnv.get[`sys:casper:return`])

  protected def mkDeployerId(pk: PublicKey) =
    "sys:casper:deployerId" ->> GDeployerId(ByteString.copyFrom(pk.bytes))

  protected def mkSysAuthToken =
    "sys:casper:authToken" ->> GSysAuthToken()

  protected val normalizerEnv: NormalizerEnv[Env]

  protected val extractor: Extractor[Output]

  val source: String

  final def extractResult[F[_]: Sync](output: Par): F[Either[SystemDeployUserError, Result]] =
    extractor.unapply(output).map(processResult).liftTo(UnexpectedResult(Seq(output)))

  protected def processResult(value: extractor.ScalaType): Either[SystemDeployUserError, Result]
}
