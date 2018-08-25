package coop.rchain.casper.util.comm

import scala.io.Source
import scala.util._
import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockQuery, Channels, DeployData, DeployServiceGrpc}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.models.{Channel, GPrivate, Par}
import coop.rchain.models.Channel.ChannelInstance

object DeployRuntime {
  sealed trait Name
  final case class PrivName(content: String) extends Name
  final case class PubName(content: String)  extends Name

  type ErrorHandler[F[_]] = ApplicativeError_[F, Throwable]

  def propose[F[_]: Monad: ErrorHandler: Capture: DeployService](): F[Unit] =
    gracefulExit(
      for {
        response <- DeployService[F].createBlock()
        _        <- Capture[F].capture { println(s"Response: ${response._2}") }
      } yield ()
    )

  def showBlock[F[_]: Monad: ErrorHandler: Capture: DeployService](hash: String): F[Unit] =
    gracefulExit(DeployService[F].showBlock(BlockQuery(hash)).map(println(_)))

  def showBlocks[F[_]: Monad: ErrorHandler: Capture: DeployService](): F[Unit] =
    gracefulExit(DeployService[F].showBlocks.map(println(_)))

  private def listenAtName[F[_]: Monad: ErrorHandler: Capture](name: Name)(
      retrieve: Par => F[String]) = {

    import coop.rchain.models.rholang.implicits._

    val par: Either[Throwable, Par] = name match {
      case PubName(content) => InterpreterUtil.mkTerm(content)
      case PrivName(content) =>
        val par: Par = GPrivate(ByteString.copyFrom(content.getBytes))
        Right(par)
    }

    val program =
      for {
        par    <- implicitly[ErrorHandler[F]].fromEither(par)
        result <- retrieve(par)
        _      <- Capture[F].capture(println(s"The result is: $result"))
      } yield ()

    gracefulExit(program)
  }

  def listenForDataAtName[F[_]: Monad: ErrorHandler: Capture: DeployService](name: Name): F[Unit] =
    listenAtName(name) { par =>
      val request = Channel(ChannelInstance.Quote(par))
      DeployService[F].listenForDataAtName(request)
    }

  def listenForContinuationAtName[F[_]: Monad: ErrorHandler: Capture: DeployService](
      name: Name): F[Unit] =
    listenAtName(name) { par =>
      val channel = Channel(ChannelInstance.Quote(par))
      val request = Channels(Seq(channel))
      DeployService[F].listenForContinuationAtName(request)
    }

  //Accepts a Rholang source file and deploys it to Casper
  def deployFileProgram[F[_]: Monad: ErrorHandler: Capture: DeployService](purseAddress: String,
                                                                           phloLimit: Int,
                                                                           phloPrice: Int,
                                                                           nonce: Int,
                                                                           file: String): F[Unit] =
    Try(Source.fromFile(file).mkString) match {
      case Success(code) =>
        gracefulExit(
          for {
            timestamp <- Capture[F].capture { System.currentTimeMillis() }
            //TODO: allow user to specify their public key
            d = DeployData()
              .withTimestamp(timestamp)
              .withTerm(code)
              .withFrom(purseAddress)
              .withPhloLimit(phloLimit)
              .withPhloPrice(phloPrice)
              .withNonce(nonce)
            response <- DeployService[F].deploy(d)
            _ <- Capture[F].capture {
                  println(s"Response: ${response._2}")
                }
          } yield ()
        )

      case Failure(ex) =>
        Capture[F].capture { println(s"Error with given file: \n${ex.getMessage()}") }
    }

  //Simulates user requests by randomly deploying things to Casper.
  def deployDemoProgram[F[_]: Monad: ErrorHandler: Capture: DeployService]: F[Unit] =
    gracefulExit(MonadOps.forever(singleDeploy[F]))

  private def singleDeploy[F[_]: Monad: Capture: DeployService]: F[Unit] =
    for {
      id <- Capture[F].capture { scala.util.Random.nextInt(100) }
      d  = ProtoUtil.basicDeployData(id)
      _ <- Capture[F].capture {
            println(s"Sending the following to Casper: ${d.term}")
          }
      response <- DeployService[F].deploy(d)
      _ <- Capture[F].capture {
            println(s"Response: ${response._2}")
          }
      _ <- IOUtil.sleep[F](4000L)
    } yield ()

  private def gracefulExit[F[_]: Monad: ErrorHandler: Capture, A](program: F[A]): F[Unit] =
    for {
      result <- program.attempt
      _ <- result match {
            case Left(ex) => Capture[F].capture(println(s"Error: ${processError(ex).getMessage}"))
            case _        => ().pure[F]
          }
    } yield ()

  private def processError(t: Throwable): Throwable =
    Option(t.getCause).getOrElse(t)

}
