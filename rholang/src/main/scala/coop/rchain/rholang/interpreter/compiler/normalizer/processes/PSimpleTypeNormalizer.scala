package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PSimpleTypeNormalizer {
  def normalize[F[_]: Sync](p: PSimpleType, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.simpletype_ match {
      case _: SimpleTypeBool =>
        ProcVisitOutputs(input.par.combine(ConnBoolN), input.freeMap).pure[F]
      case _: SimpleTypeInt =>
        ProcVisitOutputs(input.par.combine(ConnIntN), input.freeMap).pure[F]
      case _: SimpleTypeBigInt =>
        ProcVisitOutputs(input.par.combine(ConnBigIntN), input.freeMap).pure[F]
      case _: SimpleTypeString =>
        ProcVisitOutputs(input.par.combine(ConnStringN), input.freeMap).pure[F]
      case _: SimpleTypeUri =>
        ProcVisitOutputs(input.par.combine(ConnUriN), input.freeMap).pure[F]
      case _: SimpleTypeByteArray =>
        ProcVisitOutputs(input.par.combine(ConnByteArrayN), input.freeMap).pure[F]
    }
}
