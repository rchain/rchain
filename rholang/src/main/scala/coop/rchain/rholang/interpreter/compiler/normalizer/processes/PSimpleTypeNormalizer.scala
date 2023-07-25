package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PSimpleTypeNormalizer {
  def normalize[F[_]: Sync](p: PSimpleType, input: ProcVisitInputs): F[ProcVisitOutputs] = {
    val inpPar = fromProto(input.par)
    p.simpletype_ match {
      case _: SimpleTypeBool =>
        ProcVisitOutputs(toProto(inpPar.add(ConnBoolN())), input.freeMap).pure[F]
      case _: SimpleTypeInt =>
        ProcVisitOutputs(toProto(inpPar.add(ConnIntN())), input.freeMap).pure[F]
      case _: SimpleTypeBigInt =>
        ProcVisitOutputs(toProto(inpPar.add(ConnBigIntN())), input.freeMap).pure[F]
      case _: SimpleTypeString =>
        ProcVisitOutputs(toProto(inpPar.add(ConnStringN())), input.freeMap).pure[F]
      case _: SimpleTypeUri =>
        ProcVisitOutputs(toProto(inpPar.add(ConnUriN())), input.freeMap).pure[F]
      case _: SimpleTypeByteArray =>
        ProcVisitOutputs(toProto(inpPar.add(ConnByteArrayN())), input.freeMap).pure[F]
    }
  }
}
