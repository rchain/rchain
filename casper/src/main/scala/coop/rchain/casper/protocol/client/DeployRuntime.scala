package coop.rchain.casper.protocol.client

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._
import cats.{Functor, Id, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.client.ListenAtName._
import coop.rchain.catscontrib.ski.kp
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.Par
import coop.rchain.models.syntax._
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared.Time

import scala.io.Source
import scala.util.Try

object DeployRuntime {

  def propose[F[_]: Monad: Sync: ProposeService](printUnmatchedSends: Boolean): F[Unit] =
    gracefulExit(
      ProposeService[F].propose(printUnmatchedSends).map(r => r.map(rs => s"Response: $rs"))
    )

  def getBlock[F[_]: Monad: Sync: DeployService](hash: String): F[Unit] =
    gracefulExit(DeployService[F].getBlock(BlockQuery(hash)))

  def getBlocks[F[_]: Monad: Sync: DeployService](depth: Int): F[Unit] =
    gracefulExit(DeployService[F].getBlocks(BlocksQuery(depth)))

  def visualizeDag[F[_]: Monad: Sync: DeployService](
      depth: Int,
      showJustificationLines: Boolean
  ): F[Unit] =
    gracefulExit(DeployService[F].visualizeDag(VisualizeDagQuery(depth, showJustificationLines)))

  def machineVerifiableDag[F[_]: Monad: Sync: DeployService]: F[Unit] =
    gracefulExit(DeployService[F].machineVerifiableDag(MachineVerifyQuery()))

  def listenForDataAtName[F[_]: Functor: Sync: DeployService: Time](
      name: Id[Name]
  ): F[Unit] =
    gracefulExit {
      listenAtNameUntilChanges(name) { par: Par =>
        val request = DataAtNameQuery(Int.MaxValue, par)
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

  def findDeploy[F[_]: Functor: Sync: Time: DeployService](
      deployId: Array[Byte]
  ): F[Unit] =
    gracefulExit(
      DeployService[F].findDeploy(FindDeployQuery(deployId.toByteString))
    )

//Accepts a Rholang source file and deploys it to Casper
  def deployFileProgram[F[_]: Monad: Sync: DeployService](
      phloLimit: Long,
      phloPrice: Long,
      validAfterBlock: Long,
      privateKey: PrivateKey,
      file: String,
      shardId: String
  ): F[Unit] =
    gracefulExit(
      Sync[F].delay(Try(Source.fromFile(file).mkString).toEither).flatMap {
        case Left(ex) =>
          Sync[F].delay(Left(Seq(s"Error with given file: \n${ex.getMessage}")))
        case Right(code) =>
          for {
            timestamp <- Sync[F].delay(System.currentTimeMillis())

            d = DeployData(
              term = code,
              timestamp = timestamp,
              phloPrice = phloPrice,
              phloLimit = phloLimit,
              validAfterBlockNumber = validAfterBlock,
              shardId
            )

            response <- DeployService[F].deploy(Signed(d, Secp256k1, privateKey))
          } yield response.map(r => s"Response: $r")
      }
    )

  def lastFinalizedBlock[F[_]: Sync: DeployService]: F[Unit] =
    gracefulExit(DeployService[F].lastFinalizedBlock)

  def isFinalized[F[_]: Sync: DeployService](blockHash: String): F[Unit] =
    gracefulExit(DeployService[F].isFinalized(IsFinalizedQuery(hash = blockHash)))

  def bondStatus[F[_]: Sync: DeployService](publicKey: PublicKey): F[Unit] =
    gracefulExit(
      DeployService[F].bondStatus(
        BondStatusQuery(publicKey = ByteString.copyFrom(publicKey.bytes))
      )
    )

  def status[F[_]: Sync: DeployService]: F[Unit] = gracefulExit(DeployService[F].status)

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
