package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PSimpleTypeNormalizer {
  def normalize[F[_]: Sync](p: PSimpleType, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.simpletype_ match {
      case _: SimpleTypeBool =>
        ProcVisitOutputs(toProto(input.par.add(ConnBoolN())), input.freeMap).pure[F]
      case _: SimpleTypeInt =>
        ProcVisitOutputs(toProto(input.par.add(ConnIntN())), input.freeMap).pure[F]
      case _: SimpleTypeBigInt =>
        ProcVisitOutputs(toProto(input.par.add(ConnBigIntN())), input.freeMap).pure[F]
      case _: SimpleTypeString =>
        ProcVisitOutputs(toProto(input.par.add(ConnStringN())), input.freeMap).pure[F]
      case _: SimpleTypeUri =>
        ProcVisitOutputs(toProto(input.par.add(ConnUriN())), input.freeMap).pure[F]
      case _: SimpleTypeByteArray =>
        ProcVisitOutputs(toProto(input.par.add(ConnByteArrayN())), input.freeMap).pure[F]
    }
}
