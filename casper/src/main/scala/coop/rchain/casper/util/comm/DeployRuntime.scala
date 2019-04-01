package coop.rchain.casper.util.comm

import java.nio.charset.Charset

import scala.concurrent.duration._
import scala.io.Source
import scala.language.higherKinds
import scala.util._
import cats.{Functor, Id, Monad}
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.comm.ListenAtName._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.models.Par
import coop.rchain.shared.Time
import cats.syntax.either._
import com.google.protobuf.ByteString
import coop.rchain.casper.SignDeployment
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.shared.ThrowableOps._

object DeployRuntime {

  def propose[F[_]: Monad: Sync: DeployService](): F[Unit] =
    gracefulExit(
      for {
        response <- DeployService[F].createBlock()
      } yield response.map(r => s"Response: $r")
    )

  def showBlock[F[_]: Monad: Sync: DeployService](hash: String): F[Unit] =
    gracefulExit(DeployService[F].showBlock(BlockQuery(hash)))

  def showBlocks[F[_]: Monad: Sync: DeployService](depth: Int): F[Unit] =
    gracefulExit(DeployService[F].showBlocks(BlocksQuery(depth)))

  def visualizeDag[F[_]: Monad: Sync: DeployService](
      depth: Int,
      showJustificationLines: Boolean
  ): F[Unit] =
    gracefulExit(DeployService[F].visualizeDag(VisualizeDagQuery(depth, showJustificationLines)))

  def listenForDataAtName[F[_]: Functor: Sync: DeployService: Time](
      name: Id[Name]
  ): F[Unit] =
    gracefulExit {
      listenAtNameUntilChanges(name) { par: Par =>
        val request = DataAtNameQuery(Int.MaxValue, Some(par))
        EitherT(DeployService[F].listenForDataAtName(request))
      }.map(kp("")).value
    }

  def listenForContinuationAtName[F[_]: Functor: Sync: Time: DeployService](
      names: List[Name]
  ): F[Unit] =
    gracefulExit {
      listenAtNameUntilChanges(names) { pars: List[Par] =>
        val request = ContinuationAtNameQuery(Int.MaxValue, pars)
        EitherT(DeployService[F].listenForContinuationAtName(request))
      }.map(kp("")).value
    }

//Accepts a Rholang source file and deploys it to Casper
  def deployFileProgram[F[_]: Monad: Sync: DeployService](
      phloLimit: Long,
      phloPrice: Long,
      validAfterBlock: Int,
      maybePrivateKeyFile: Option[String],
      file: String
  ): F[Unit] = {
    val readInputFiles: F[Either[Throwable, (String, Option[PrivateKey])]] =
      Sync[F]
        .delay(
          for {
            code <- Try(Source.fromFile(file).mkString).toEither
            maybePrivateKeyBase16 <- maybePrivateKeyFile
                                      .traverse(f => Try(Source.fromFile(f).mkString).toEither)

            maybePrivateKey <- maybePrivateKeyBase16.traverse(
                                k =>
                                  Base16
                                    .decode(k.toLowerCase)
                                    .map(PrivateKey(_))
                                    .toRight(
                                      new Exception(
                                        s"Error parsing private key file. Invalid base16 encoding."
                                      )
                                    )
                              )
          } yield (code, maybePrivateKey)
        )

    def doDeploy(
        code: String,
        maybePrivateKey: Option[PrivateKey]
    ): F[Either[Seq[String], String]] =
      for {
        timestamp <- Sync[F].delay(System.currentTimeMillis())

        //TODO: allow user to specify their public key
        d = DeployData()
          .withTimestamp(timestamp)
          .withTerm(code)
          .withPhloLimit(phloLimit)
          .withPhloPrice(phloPrice)
          .withValidAfterBlockNumber(validAfterBlock)
          .withTimestamp(timestamp)

        signedData = maybePrivateKey.fold(d)(SignDeployment.sign(_, d))

        response <- DeployService[F].deploy(signedData)
      } yield response.map(r => s"Response: $r")

    gracefulExit(
      readInputFiles
        .flatMap {
          case Left(ex) =>
            Sync[F].delay(Left(Seq(s"Error with given file: \n${ex.getMessage}")))
          case Right((code, maybePrivateKey)) =>
            doDeploy(code, maybePrivateKey)
        }
    )
  }

  private def gracefulExit[F[_]: Monad: Sync, A](
      program: F[Either[Seq[String], String]]
  ): F[Unit] =
    for {
      result <- Sync[F].attempt(program)
      _ <- processError(result).joinRight match {
            case Left(errors) =>
              Sync[F].delay {
                errors.foreach(error => println(error))
                System.exit(1)
              }
            case Right(msg) => Sync[F].delay(println(msg))
          }
    } yield ()

  private def processError[A](error: Either[Throwable, A]): Either[Seq[String], A] =
    error.leftMap(_.toMessageList())

}
