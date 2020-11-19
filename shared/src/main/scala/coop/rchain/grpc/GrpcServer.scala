package coop.rchain.grpc

import java.util.concurrent.TimeUnit

import cats.effect.Sync
import cats.syntax.all._

import coop.rchain.catscontrib.ski.kp

trait Server[F[_]] {
  def start: F[Unit]
  def stop: F[Unit]
  def port: Int
}

class GrpcServer[F[_]: Sync](server: io.grpc.Server) extends Server[F] {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def start: F[Unit] = Sync[F].delay(server.start())

  private def attemptShutdown: F[Boolean] =
    (for {
      _          <- Sync[F].delay(server.shutdown())
      _          <- Sync[F].delay(server.awaitTermination(1000, TimeUnit.MILLISECONDS))
      terminated <- Sync[F].delay(server.isTerminated)
    } yield terminated).attempt map (_.fold(kp(false), identity))

  private def shutdownImmediately: F[Unit] =
    Sync[F].delay(server.shutdownNow()).attempt.as(())

  def stop: F[Unit] = attemptShutdown >>= { stopped =>
    if (stopped) Sync[F].unit else shutdownImmediately
  }
  def port: Int = server.getPort
}

object GrpcServer {
  def apply[F[_]: Sync](server: io.grpc.Server): F[Server[F]] =
    Sync[F].delay(new GrpcServer[F](server))
}
