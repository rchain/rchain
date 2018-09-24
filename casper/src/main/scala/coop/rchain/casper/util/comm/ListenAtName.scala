package coop.rchain.casper.util.comm
import cats.effect.{Sync, Timer}
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Id, MonadError}
import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.models.{GPrivate, Par}

import scala.language.higherKinds
import scala.util.{Either, Right}

object ListenAtName {
  sealed trait Name
  final case class PrivName(content: String) extends Name
  final case class PubName(content: String)  extends Name

  trait BuildPar[F[_]] {
    def build(f: F[Name]): F[Par]
  }

  private def buildPar[G[_]: BuildPar](name: G[Name]) = implicitly[BuildPar[G]].build(name)

  implicit def buildParF[F[_]: MonadError[?[_], Throwable]] = new BuildPar[λ[A => F[Id[A]]]] {
    override def build(f: F[Name]) =
      for {
        name <- f
        res  <- buildParId(name)
      } yield res
  }

  implicit def buildParListF[F[_]: MonadError[?[_], Throwable]] =
    new BuildPar[λ[A => F[List[A]]]] {
      override def build(f: F[List[Name]]): F[List[Par]] =
        f.flatMap(_.traverse(buildParId[F]))
    }

  private def buildParId[F[_]: MonadError[?[_], Throwable]](name: Name): F[Par] = {
    import coop.rchain.models.rholang.implicits._

    val par: Either[Throwable, Par] = name match {
      case PubName(content) =>
        InterpreterUtil.mkTerm(content)
      case PrivName(content) =>
        val par: Par = GPrivate(ByteString.copyFrom(content.getBytes))
        Right(par)
    }

    MonadError[F, Throwable].fromEither(par)
  }

  private def applyUntil[A, F[_]: Sync: Timer](retrieve: F[A])(breakCond: A => Boolean): F[A] = {
    import scala.concurrent.duration._

    def loop: F[A] =
      for {
        _    <- implicitly[Timer[F]].sleep(1.second)
        data <- retrieve
        res <- if (breakCond(data)) data.pure[F]
              else loop
      } yield res

    loop
  }

  def listenAtNameUntilChanges[A, G[_], F[_]: Sync: Timer](
      name: G[Name]
  )(request: G[Par] => F[Seq[A]])(implicit par: BuildPar[λ[A => F[G[A]]]]) = {
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
