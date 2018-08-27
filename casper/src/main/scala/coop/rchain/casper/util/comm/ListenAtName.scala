package coop.rchain.casper.util.comm
import cats.Monad
import cats.effect.{Sync, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.google.protobuf.ByteString
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.models.{GPrivate, Par}

import scala.language.higherKinds
import scala.util.{Either, Right}

object ListenAtName {
  sealed trait Name {
    val content: String
  }
  final case class PrivName(content: String) extends Name
  final case class PubName(content: String)  extends Name

  private def buildPar[F[_]: Sync](name: Name): F[Par] = {
    import coop.rchain.models.rholang.implicits._

    val par: Either[Throwable, Par] = name match {
      case PubName(content) =>
        InterpreterUtil.mkTerm(content)
      case PrivName(content) =>
        val par: Par = GPrivate(ByteString.copyFrom(content.getBytes))
        Right(par)
    }

    Sync[F].fromEither(par)
  }

  private def applyUntil[A, F[_]: Sync: Timer](retrieve: F[A])(breakCond: A => Boolean): F[A] = {
    import scala.concurrent.duration._

    def loop: F[A] =
      for {
        _    <- Timer[F].sleep(1.second)
        data <- retrieve
        res <- if (breakCond(data)) Monad[F].pure(data)
              else loop
      } yield res

    loop
  }

  def listenAtNameUntilChanges[A, F[_]: Sync: Timer](name: Name)(request: Par => F[Seq[A]]) = {

    val retrieve =
      for {
        par  <- buildPar(name)
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
