package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.{
  ConnBool,
  ConnByteArray,
  ConnInt,
  ConnString,
  ConnUri
}
import coop.rchain.models.{Connective, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  PSimpleType,
  SimpleTypeBool,
  SimpleTypeByteArray,
  SimpleTypeInt,
  SimpleTypeString,
  SimpleTypeUri
}

object PSimpleTypeNormalizer {
  def normalize[F[_]: Sync](p: PSimpleType, input: ProcVisitInputs): F[ProcVisitOutputs] =
    p.simpletype_ match {
      case _: SimpleTypeBool =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnBool(true)), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree
        ).pure[F]
      case _: SimpleTypeInt =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnInt(true)), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree
        ).pure[F]
      case _: SimpleTypeString =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnString(true)), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree
        ).pure[F]
      case _: SimpleTypeUri =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnUri(true)), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree
        ).pure[F]
      case _: SimpleTypeByteArray =>
        ProcVisitOutputs(
          input.par
            .prepend(Connective(ConnByteArray(true)), input.env.depth)
            .withConnectiveUsed(true),
          input.knownFree
        ).pure[F]
    }
}
