package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.{
  ConnBool,
  ConnByteArray,
  ConnInt,
  ConnString,
  ConnUri
}
import coop.rchain.models.rholang.implicits.ParExtension
import coop.rchain.models.{Connective, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  PSimpleType,
  SimpleTypeBool,
  SimpleTypeByteArray,
  SimpleTypeInt,
  SimpleTypeString,
  SimpleTypeUri
}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.normalizer.Normalizer

trait PSimpleTypeInstance {
  implicit def PSimpleTypeInstance[F[_]: Sync]
      : Normalizer[F, PSimpleType, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PSimpleType, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PSimpleType, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] = p.simpletype_ match {
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
}
