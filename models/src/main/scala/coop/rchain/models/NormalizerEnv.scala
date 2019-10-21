package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PublicKey

import scala.annotation.implicitNotFound

final class NormalizerEnv[Env](val env: Env) {
  import NormalizerEnv._
  def toEnv(implicit ToEnvMap: ToEnvMap[Env]): Map[String, Par] = ToEnvMap(env)
  def provide[Env0](implicit ev: Provides[Env, Env0]): NormalizerEnv[Env0] =
    new NormalizerEnv[Env0](ev.select(env))
}

object NormalizerEnv {
  import shapeless._
  import syntax.singleton._
  import ops.record.{Extractor, Keys, Values}
  import ops.hlist.ToList

  type UriString = String

  val Empty: NormalizerEnv[HNil] = new NormalizerEnv(HNil)

  def withDeployId(deployId: Array[Byte]) =
    new NormalizerEnv(("rho:rchain:deployId" ->> GDeployId(ByteString.copyFrom(deployId))) :: HNil)

  def withDeployerId(deployerPk: PublicKey) =
    new NormalizerEnv(
      ("rho:rchain:deployerId" ->> GDeployerId(ByteString.copyFrom(deployerPk.bytes))) :: HNil
    )

  def apply(deploy: DeployData) =
    new NormalizerEnv(
      ("rho:rchain:deployId" ->> GDeployId(deploy.sig)) ::
        ("rho:rchain:deployerId" ->> GDeployerId(deploy.deployer)) :: HNil
    )

  @implicitNotFound(
    "Elements of ${L} should be Pars. Maybe you forgot to import coop.rchain.models.rholang.implicits._ ?"
  )
  trait ToParList[L <: HList] {
    def apply(l: L): List[Par]
  }

  object ToParList {
    implicit val hNilToParList: ToParList[HNil] = (l: HNil) => Nil

    implicit def hSingleToParList[T](implicit ev: T => Par): ToParList[T :: HNil] =
      (l: T :: HNil) => ev(l.head) :: Nil

    implicit def hListToParList[H, T <: HList](
        implicit ev: H => Par,
        tailToParList: ToParList[T]
    ): ToParList[H :: T] = (l: H :: T) => ev(l.head) :: tailToParList(l.tail)
  }

  @implicitNotFound(
    "${Env} must consist of URI strings and Par expressions. Maybe you forgot to import coop.rchain.models.rholang.implicits._ ?"
  )
  trait ToEnvMap[Env] {
    def apply(env: Env): Map[String, Par]
  }

  object ToEnvMap {
    def apply[Env](implicit ev: ToEnvMap[Env]) = ev

    implicit def summon[K <: HList, V <: HList, Env <: HList](
        implicit K: Keys.Aux[Env, K],
        V: Values.Aux[Env, V],
        keysList: ToList[K, String],
        parList: ToParList[V]
    ): ToEnvMap[Env] = new ToEnvMap[Env] {
      override def apply(env: Env): Map[String, Par] = {
        val uris = keysList(K())
        val pars = parList(V(env))
        uris.iterator.zip(pars.iterator).toMap
      }
    }
  }

  @implicitNotFound("${Env0} does not provide ${Env1}")
  trait Provides[Env0, Env1] {
    def select(env0: Env0): Env1
  }

  object Provides {
    implicit def summon[Env0 <: HList, Env1 <: HList](
        implicit extractor: Extractor[Env0, Env1]
    ): Provides[Env0, Env1] =
      (env0: Env0) => extractor(env0)
  }

}
