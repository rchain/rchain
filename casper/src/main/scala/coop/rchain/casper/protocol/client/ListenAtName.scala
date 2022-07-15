package coop.rchain.casper.protocol.client

import cats.Id
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.rholang.InterpreterUtil
import coop.rchain.models.rholang.RhoType.Name
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.shared.Time

object ListenAtName {
  sealed trait Name
  final case class PrivName(content: String) extends Name
  final case class PubName(content: String)  extends Name

  trait BuildPar[F[_]] {
    def build(f: F[Name]): F[Par]
  }

  private def buildPar[G[_]: BuildPar](name: G[Name]) = implicitly[BuildPar[G]].build(name)

  implicit def buildParF[F[_]: Sync] = new BuildPar[λ[A => F[Id[A]]]] {
    override def build(f: F[Name]) =
      for {
        name <- f
        res  <- buildParId(name)
      } yield res
  }

  implicit def buildParListF[F[_]: Sync] =
    new BuildPar[λ[A => F[List[A]]]] {
      override def build(f: F[List[Name]]): F[List[Par]] =
        f.flatMap(_.traverse(buildParId[F]))
    }

  private def buildParId[F[_]: Sync](name: Name): F[Par] =
    name match {
      case PubName(content) =>
        InterpreterUtil.mkTerm(content, NormalizerEnv.Empty)
      case PrivName(content) =>
        Sync[F].delay {
          Name(content.getBytes)
        }
    }

  private def applyUntil[A, F[_]: Sync: Time](retrieve: F[A])(breakCond: A => Boolean): F[A] = {
    import scala.concurrent.duration._

    def loop: F[A] =
      for {
        _    <- Time[F].sleep(1.second)
        data <- retrieve
        res <- if (breakCond(data)) data.pure[F]
              else loop
      } yield res

    loop
  }

  def listenAtNameUntilChanges[A1, G[_], F[_]: Sync: Time](
      name: G[Name]
  )(request: G[Par] => F[Seq[A1]])(implicit par: BuildPar[λ[A => F[G[A]]]]): F[Unit] = {
    val nameF = name.pure[F]

    val retrieve =
      for {
        par  <- buildPar[λ[A => F[G[A]]]](nameF)
        init <- request(par)
      } yield init

    for {
      _ <- Sync[F].delay(println(s"Listen at name: $name"))
      _ <- Sync[F].delay(println("Start monitoring for changes"))

      init   <- retrieve
      result <- applyUntil(retrieve)(_.size > init.size)
      _      <- Sync[F].delay(println("Detected changes:"))
      _      <- Sync[F].delay(println(result.drop(init.size - result.size)))
    } yield ()
  }
}
