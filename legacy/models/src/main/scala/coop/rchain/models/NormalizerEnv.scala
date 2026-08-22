package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed

import scala.annotation.implicitNotFound

/** TODO: Check why environment is needed in normalizer? It's used only in reducer. */
final class NormalizerEnv[Env](env: Env) {
  import NormalizerEnv._
  def toEnv(implicit ToEnvMap: ToEnvMap[Env]): Map[String, Par]      = ToEnvMap(env)
  def get[T](implicit ev: Contains[Env, T]): ev.KeyType              = ev.get(env)
  def get(uri: shapeless.Witness)(implicit ev: Contains[Env, uri.T]) = get[uri.T]
}

object NormalizerEnv {
  import shapeless._
  import ops.hlist.ToList
  import ops.record.{Keys, Selector, Values}
  import shapeless.syntax.singleton._

  type UriString = String

  val Empty: NormalizerEnv[HNil] = new NormalizerEnv(HNil)

  def withDeployerId(deployerPk: PublicKey) =
    new NormalizerEnv(
      ("rho:rchain:deployerId" ->> GDeployerId(ByteString.copyFrom(deployerPk.bytes))) :: HNil
    )

  def apply(deploy: Signed[DeployData]) =
    new NormalizerEnv(
      ("rho:rchain:deployId" ->> GDeployId(deploy.sig)) ::
        ("rho:rchain:deployerId" ->> GDeployerId(ByteString.copyFrom(deploy.pk.bytes))) :: HNil
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

  @implicitNotFound("${Env} does not contain ${Key}")
  trait Contains[Env, Key] {
    type KeyType

    def get(env: Env): KeyType
  }

  object Contains {
    type Aux[Env, Key, T] = Contains[Env, Key] { type KeyType = T }

    def apply[Env, Key](implicit ev: Contains[Env, Key]): Contains.Aux[Env, Key, ev.KeyType] = ev

    implicit def summon[Env <: HList, Key, T](
        implicit selector: Selector.Aux[Env, Key, T]
    ): Aux[Env, Key, T] = new Contains[Env, Key] {
      type KeyType = T
      def get(env: Env): T = selector(env)
    }
  }
}
