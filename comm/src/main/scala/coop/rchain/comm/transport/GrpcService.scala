package coop.rchain.comm.transport

import scala.util.{Failure, Try}

import coop.rchain.catscontrib.ski._
import coop.rchain.comm.protocol.routing.{TLRequest, TLResponse}
import coop.rchain.comm.protocol.routing.TransportLayerGrpc._
import coop.rchain.shared.{Log, LogSource}

import io.grpc._
import io.grpc.stub.{ServerCalls, StreamObserver}
import monix.eval.Task
import monix.execution.Scheduler

/*
  This is a replacement object for TransportLayerGrpc and allows a fine-grained
  control over the service request-response cycle in gRPC.
 */
object GrpcService {
  def bindService(transportLayer: TransportLayer)(implicit scheduler: Scheduler,
                                                  log: Log[Task],
                                                  logSource: LogSource): ServerServiceDefinition =
    ServerServiceDefinition
      .builder(SERVICE)
      .addMethod(
        METHOD_SEND,
        ServerCalls.asyncUnaryCall(new ServerCalls.UnaryMethod[TLRequest, TLResponse] {
          override def invoke(request: TLRequest, observer: StreamObserver[TLResponse]): Unit =
            transportLayer
              .send(request)
              .materialize
              .flatMap(response =>
                Try(scalapb.grpc.Grpc.completeObserver(observer)(response)) match {
                  /*
                      When the peer node gets shutdown before the service can respond
                      grpc-java throws an Exception which gets printed to the console.
                      This exception shouldn't appear on console because it's a very
                      common situation in a P2P network.
                   */
                  case Failure(e: StatusRuntimeException)
                      if e.getStatus.getCode == Status.Code.CANCELLED =>
                    log.debug("Couldn't send response: Channel was cancelled")
                  case Failure(e) =>
                    log
                      .error(s"Couldn't send response: ${e.getMessage}")
                      .map(kp(e.printStackTrace()))
                  case _ => Task.unit
              })
              .runAsync
        })
      )
      .build()

  trait TransportLayer {
    def send(request: TLRequest): Task[TLResponse]
  }
}
