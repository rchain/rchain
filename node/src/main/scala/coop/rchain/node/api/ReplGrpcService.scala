package coop.rchain.node.api

import cats.effect.Sync
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.node.model.{CmdRequest, EvalRequest, ReplFs2Grpc, ReplResponse}
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import io.grpc.Metadata
import cats.syntax.all._

object ReplGrpcService {

  def apply[F[_]: Sync](runtime: RhoRuntime[F]): ReplFs2Grpc[F, Metadata] =
    new ReplFs2Grpc[F, Metadata] {
      def exec(source: String, printUnmatchedSendsOnly: Boolean = false): F[ReplResponse] =
        Sync[F]
          .attempt(
            Compiler[F]
              .sourceToADT(source, Map.empty[String, Par])
          )
          .flatMap {
            case Left(er) =>
              er match {
                case _: InterpreterError => Sync[F].delay(s"Error: ${er.toString}")
                case th: Throwable       => Sync[F].delay(s"Error: $th")
              }
            case Right(term) =>
              for {
                _ <- Sync[F].delay(printNormalizedTerm(term))
                res <- {
                  val rand = Blake2b512Random.defaultRandom
                  runtime.evaluate(source, Cost.UNSAFE_MAX, Map.empty[String, Par], rand)
                }
                prettyStorage <- if (printUnmatchedSendsOnly)
                                  StoragePrinter.prettyPrintUnmatchedSends(runtime)
                                else StoragePrinter.prettyPrint(runtime)
                EvaluateResult(cost, errors, _) = res
              } yield {
                val errorStr =
                  if (errors.isEmpty)
                    ""
                  else
                    errors
                      .map(_.toString())
                      .mkString("Errors received during evaluation:\n", "\n", "\n")
                s"Deployment cost: $cost\n" +
                  s"${errorStr}Storage Contents:\n$prettyStorage"
              }
          }
          .map(ReplResponse(_))

      def run(request: CmdRequest, ctx: Metadata): F[ReplResponse] =
        exec(request.line)

      def eval(request: EvalRequest, ctx: Metadata): F[ReplResponse] =
        exec(request.program, request.printUnmatchedSendsOnly)

      private def printNormalizedTerm(normalizedTerm: Par): Unit = {
        Console.println("\nEvaluating:")
        Console.println(PrettyPrinter().buildString(normalizedTerm))
      }
    }
}
