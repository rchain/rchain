package coop.rchain.casper.util.comm

import com.google.protobuf.ByteString

import cats.{Functor, Monad}
import cats.implicits._
import coop.rchain.casper.protocol.{BlockMessage, BlockQuery, DeployString}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.{Capture, IOUtil, MonadOps}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object DeployRuntime {

  //Propose a block, sign using given secret key with Ed25519.
  //Note that this is just an example thin-client making use of the
  //gRPC functionalities that are exposed. A node operator could easily
  //write their own (e.g. in Python) if they wish to handle signing differently.
  def propose[F[_]: DeployService: Monad](sk: Array[Byte]): F[Unit] =
    DeployService[F].createBlock().flatMap {
      case Some(block) =>
        val signedBlock = ProtoUtil.signBlock(block, sk)
        DeployService[F].addBlock(signedBlock)

      case None =>
        ().pure[F]
    }

  def showBlock[F[_]: Functor: DeployService](hash: String): F[Unit] =
    DeployService[F].showBlock(BlockQuery(hash)).map(println(_))

  def showBlocks[F[_]: Functor: DeployService](): F[Unit] =
    DeployService[F].showBlocks.map(println(_))

  //Accepts a Rholang source file and deploys it to Casper
  def deployFileProgram[F[_]: Monad: Capture: DeployService](file: String): F[Unit] =
    Try(Source.fromFile(file).mkString) match {
      case Success(code) =>
        for {
          timestamp <- Capture[F].capture { System.nanoTime() }
          //TODO: allow user to specify their public key
          d        = DeployString().withTimestamp(timestamp).withTerm(code)
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
      d  = ProtoUtil.basicDeployString(id)
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
