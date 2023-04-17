package coop.rchain.node

import cats.effect.std.Dispatcher
import cats.effect.{Async, Resource, Sync}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceFs2Grpc
import coop.rchain.casper.protocol.propose.v1.ProposeServiceFs2Grpc
import coop.rchain.node.model.ReplFs2Grpc
import coop.rchain.shared._
import io.grpc
import io.grpc.Metadata
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService

import java.net.InetSocketAddress
import scala.concurrent.duration.{FiniteDuration, SECONDS}

package object api {

  def acquireInternalServer[F[_]: Async](
      host: String,
      port: Int,
      replService: ReplFs2Grpc[F, Metadata],
      deployService: DeployServiceFs2Grpc[F, Metadata],
      proposeService: ProposeServiceFs2Grpc[F, Metadata],
      maxMessageSize: Int,
      keepAliveTime: FiniteDuration,
      keepAliveTimeout: FiniteDuration,
      permitKeepAliveTime: FiniteDuration,
      maxConnectionIdle: FiniteDuration,
      maxConnectionAge: FiniteDuration,
      maxConnectionAgeGrace: FiniteDuration
  ): Resource[F, grpc.Server] =
    Dispatcher.parallel[F].flatMap { d =>
      val server = NettyServerBuilder
        .forAddress(new InetSocketAddress(host, port))
        .maxInboundMessageSize(maxMessageSize)
        .addService(ReplFs2Grpc.bindService(d, replService))
        .addService(ProposeServiceFs2Grpc.bindService(d, proposeService))
        .addService(DeployServiceFs2Grpc.bindService(d, deployService))
        .addService(ProtoReflectionService.newInstance())
        .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
        .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
        .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
        .maxConnectionIdle(60, SECONDS)
        .maxConnectionAge(maxConnectionAge.length, maxConnectionAge.unit)
        .maxConnectionAgeGrace(60, SECONDS)
        .compressorRegistry(null)
        .build

      Resource.make(Sync[F].delay(server.start))(s => Sync[F].delay(s.shutdown.awaitTermination()))
    }

  def acquireExternalServer[F[_]: Async: Log](
      host: String,
      port: Int,
      deployGrpcService: DeployServiceFs2Grpc[F, Metadata],
      maxMessageSize: Int,
      keepAliveTime: FiniteDuration,
      keepAliveTimeout: FiniteDuration,
      permitKeepAliveTime: FiniteDuration,
      maxConnectionIdle: FiniteDuration,
      maxConnectionAge: FiniteDuration,
      maxConnectionAgeGrace: FiniteDuration
  ): Resource[F, grpc.Server] =
    Dispatcher.parallel[F].flatMap { d =>
      val server = NettyServerBuilder
        .forAddress(new InetSocketAddress(host, port))
        .maxInboundMessageSize(maxMessageSize)
        .addService(DeployServiceFs2Grpc.bindService(d, deployGrpcService))
        .compressorRegistry(null)
        .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
        .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
        .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
        .maxConnectionIdle(maxConnectionIdle.length, maxConnectionIdle.unit)
        .maxConnectionAge(maxConnectionAge.length, maxConnectionAge.unit)
        .maxConnectionAgeGrace(maxConnectionAgeGrace.length, maxConnectionAgeGrace.unit)
        .addService(ProtoReflectionService.newInstance())
        .build

      Resource.make(Sync[F].delay(server.start))(s => Sync[F].delay(s.shutdown.awaitTermination()))
    }
}
