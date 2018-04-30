package coop.rchain.casper.util.comm

import cats.Monad
import cats.implicits._

import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.{Capture, IOUtil, MonadOps}

//Simulates user requests by randomly deploying things to Casper.
//TODO: replace with proper service to handle deploy requests
object DeployRuntime {
  def deployProgram[F[_]: Monad: Capture: DeployService]: F[Unit] =
    MonadOps.forever(singleDeploy[F])

  private def singleDeploy[F[_]: Monad: Capture: DeployService]: F[Unit] =
    for {
      id <- Capture[F].capture { scala.util.Random.nextInt(100) }
      d  = ProtoUtil.basicDeploy(id)
      _  <- DeployService[F].deploy(d)
      _  <- IOUtil.sleep[F](4000L)
    } yield ()
}
