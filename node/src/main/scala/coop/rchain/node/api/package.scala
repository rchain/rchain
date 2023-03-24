package coop.rchain.node

import cats.effect.{Concurrent, ConcurrentEffect, Resource, Sync}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceFs2Grpc
import coop.rchain.casper.protocol.propose.v1.ProposeServiceFs2Grpc
import coop.rchain.node.model.ReplFs2Grpc
import coop.rchain.shared._
import io.grpc
import io.grpc.Metadata
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.execution.Scheduler

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

package object api {

  def acquireInternalServer[F[_]: Sync: ConcurrentEffect](
      host: String,
      port: Int,
      grpcExecutor: Scheduler,
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
  ): Resource[F, grpc.Server] = {
    val server = NettyServerBuilder
      .forAddress(new InetSocketAddress(host, port))
      .executor(grpcExecutor)
      .maxInboundMessageSize(maxMessageSize)
      .addService(ReplFs2Grpc.bindService(replService))
      .addService(ProposeServiceFs2Grpc.bindService(proposeService))
      .addService(DeployServiceFs2Grpc.bindService(deployService))
      .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
      .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
      .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
      .maxConnectionIdle(maxConnectionIdle.length, maxConnectionIdle.unit)
      .maxConnectionAge(maxConnectionAge.length, maxConnectionAge.unit)
      .maxConnectionAgeGrace(maxConnectionAgeGrace.length, maxConnectionAgeGrace.unit)
      .addService(ProtoReflectionService.newInstance())
      .compressorRegistry(null)
      .build

    Resource.make(Sync[F].delay(server.start))(s => Sync[F].delay(s.shutdown.awaitTermination()))
  }

  def acquireExternalServer[F[_]: Concurrent: ConcurrentEffect: Log](
      host: String,
      port: Int,
      grpcExecutor: Scheduler,
      deployGrpcService: DeployServiceFs2Grpc[F, Metadata],
      maxMessageSize: Int,
      keepAliveTime: FiniteDuration,
      keepAliveTimeout: FiniteDuration,
      permitKeepAliveTime: FiniteDuration,
      maxConnectionIdle: FiniteDuration,
      maxConnectionAge: FiniteDuration,
      maxConnectionAgeGrace: FiniteDuration
  ): Resource[F, grpc.Server] = {
    val server = NettyServerBuilder
      .forAddress(new InetSocketAddress(host, port))
      .executor(grpcExecutor)
      .maxInboundMessageSize(maxMessageSize)
      .addService(DeployServiceFs2Grpc.bindService(deployGrpcService))
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
