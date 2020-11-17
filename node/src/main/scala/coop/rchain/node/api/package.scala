package coop.rchain.node

import java.net.InetSocketAddress

import cats.effect.{Concurrent, Sync}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.node.model.repl._
import coop.rchain.shared._
import io.grpc.netty.NettyServerBuilder
import io.grpc.protobuf.services.ProtoReflectionService
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration

package object api {

  def acquireInternalServer[F[_]: Sync](
      host: String,
      port: Int,
      grpcExecutor: Scheduler,
      replGrpcService: ReplGrpcMonix.Repl,
      deployGrpcService: DeployServiceV1GrpcMonix.DeployService,
      proposeGrpcService: ProposeServiceV1GrpcMonix.ProposeService,
      maxMessageSize: Int,
      keepAliveTime: FiniteDuration,
      keepAliveTimeout: FiniteDuration,
      permitKeepAliveTime: FiniteDuration,
      maxConnectionIdle: FiniteDuration,
      maxConnectionAge: FiniteDuration,
      maxConnectionAgeGrace: FiniteDuration
  ): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forAddress(new InetSocketAddress(host, port))
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          ReplGrpcMonix.bindService(replGrpcService, grpcExecutor)
        )
        .addService(
          ProposeServiceV1GrpcMonix
            .bindService(proposeGrpcService, grpcExecutor)
        )
        .addService(
          DeployServiceV1GrpcMonix
            .bindService(deployGrpcService, grpcExecutor)
        )
        .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
        .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
        .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
        .maxConnectionIdle(maxConnectionIdle.length, maxConnectionIdle.unit)
        .maxConnectionAge(maxConnectionAge.length, maxConnectionAge.unit)
        .maxConnectionAgeGrace(maxConnectionAgeGrace.length, maxConnectionAgeGrace.unit)
        .addService(ProtoReflectionService.newInstance())
        .compressorRegistry(null)
        .build
    )

  def acquireExternalServer[F[_]: Concurrent: Log](
      host: String,
      port: Int,
      grpcExecutor: Scheduler,
      deployGrpcService: DeployServiceV1GrpcMonix.DeployService,
      maxMessageSize: Int,
      keepAliveTime: FiniteDuration,
      keepAliveTimeout: FiniteDuration,
      permitKeepAliveTime: FiniteDuration,
      maxConnectionIdle: FiniteDuration,
      maxConnectionAge: FiniteDuration,
      maxConnectionAgeGrace: FiniteDuration
  ): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forAddress(new InetSocketAddress(host, port))
        .executor(grpcExecutor)
        .maxMessageSize(maxMessageSize)
        .addService(
          DeployServiceV1GrpcMonix
            .bindService(deployGrpcService, grpcExecutor)
        )
        .compressorRegistry(null)
        .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
        .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
        .permitKeepAliveTime(permitKeepAliveTime.length, permitKeepAliveTime.unit)
        .maxConnectionIdle(maxConnectionIdle.length, maxConnectionIdle.unit)
        .maxConnectionAge(maxConnectionAge.length, maxConnectionAge.unit)
        .maxConnectionAgeGrace(maxConnectionAgeGrace.length, maxConnectionAgeGrace.unit)
        .addService(ProtoReflectionService.newInstance())
        .build
    )
}
