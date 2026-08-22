package coop.rchain.grpc.syntax

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration

import io.grpc.stub.AbstractStub

trait StubSyntax {
  implicit final def grpcSyntaxStub[S <: AbstractStub[S]](stub: S): StubOps[S] =
    new StubOps[S](stub)
}

final class StubOps[S <: AbstractStub[S]](val stub: S) extends AnyVal {
  def withDeadlineAfter(duration: FiniteDuration): S =
    stub.withDeadlineAfter(duration.toNanos, TimeUnit.NANOSECONDS)
}
