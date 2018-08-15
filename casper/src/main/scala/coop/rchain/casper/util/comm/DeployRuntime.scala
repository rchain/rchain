package coop.rchain.casper.util.comm

import cats.{Functor, Monad}
import cats.implicits._
import coop.rchain.casper.protocol.{BlockQuery, DeployData}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.{Capture, IOUtil, MonadOps}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object DeployRuntime {

  def propose[F[_]: Monad: Capture: DeployService](): F[Unit] =
    for {
      response <- DeployService[F].createBlock()
      _        <- Capture[F].capture { println(s"Response: ${response._2}") }
    } yield ()

  def showBlock[F[_]: Functor: DeployService](hash: String): F[Unit] =
    DeployService[F].showBlock(BlockQuery(hash)).map(println(_))

  def showBlocks[F[_]: Functor: DeployService](): F[Unit] =
    DeployService[F].showBlocks.map(println(_))

  //Accepts a Rholang source file and deploys it to Casper
  def deployFileProgram[F[_]: Monad: Capture: DeployService](purseAddress: String,
                                                             phloLimit: Int,
                                                             phloPrice: Int,
                                                             nonce: Int,
                                                             file: String): F[Unit] =
    Try(Source.fromFile(file).mkString) match {
      case Success(code) =>
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

      case Failure(ex) =>
        Capture[F].capture { println(s"Error with given file: \n${ex.getMessage()}") }
    }

  //Simulates user requests by randomly deploying things to Casper.
  def deployDemoProgram[F[_]: Monad: Capture: DeployService]: F[Unit] =
    MonadOps.forever(singleDeploy[F])

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
}
