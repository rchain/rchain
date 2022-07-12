package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}

object PSimpleTypeNormalizer {
  def normalize[F[_]: Sync](p: PSimpleType, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.simpletype_ match {
      case _: SimpleTypeBool =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnBool(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
      case _: SimpleTypeInt =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnInt(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
      case _: SimpleTypeBigInt =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnBigInt(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
      case _: SimpleTypeString =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnString(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
      case _: SimpleTypeUri =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnUri(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
      case _: SimpleTypeByteArray =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnByteArray(true)), input.boundMapChain.depth)
            .withConnectiveUsed(true),
          input.freeMap
        ).pure[F]
    }
}
