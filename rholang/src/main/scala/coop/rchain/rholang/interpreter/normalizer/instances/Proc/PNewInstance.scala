package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.{New, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{NameDeclSimpl, NameDeclUrn, PNew, Proc}
import coop.rchain.rholang.interpreter.compiler.{NameSort, SourcePosition}
import coop.rchain.rholang.interpreter.compiler.Visit._
import coop.rchain.rholang.interpreter.errors.NormalizerError
import coop.rchain.rholang.interpreter.normalizer.Normalizer

import scala.collection.convert.ImplicitConversionsToScala._

trait PNewInstance {
  implicit def PNewInstance[F[_]: Sync]
      : Normalizer[F, PNew, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PNew, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PNew, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] = {

        val deployIdUri   = "rho:rchain:deployId"
        val deployerIdUri = "rho:rchain:deployerId"
        // TODO: bindings within a single new shouldn't have overlapping names.
        val newTaggedBindings = p.listnamedecl_.toVector.map {
          case n: NameDeclSimpl => (None, n.var_, NameSort, n.line_num, n.col_num)
          case n: NameDeclUrn =>
            (
              Some(stripUri(n.uriliteral_)),
              n.var_,
              NameSort,
              n.line_num,
              n.col_num
            )
        }
        // This sorts the None's first, and the uris by lexicographical order.
        // We do this here because the sorting affects the numbering of variables inside the body.
        val sortBindings       = newTaggedBindings.sortBy(row => row._1)
        val newBindings        = sortBindings.map(row => (row._2, row._3, SourcePosition(row._4, row._5)))
        val uris               = sortBindings.flatMap(row => row._1)
        val newEnv             = input.env.put(newBindings.toList)
        val newCount           = newEnv.count - input.env.count
        val requiresDeployId   = uris.contains(deployIdUri)
        val requiresDeployerId = uris.contains(deployerIdUri)

        def missingEnvElement(name: String, uri: String) =
          NormalizerError(s"`$uri` was used in rholang usage context where $name is not available.")
        if (requiresDeployId && env.get(deployIdUri).forall(_.singleDeployId().isEmpty))
          missingEnvElement("DeployId", deployIdUri).raiseError[F, ProcVisitOutputs[Par]]
        else if (requiresDeployerId && env.get(deployerIdUri).forall(_.singleDeployerId().isEmpty))
          missingEnvElement("DeployerId", deployerIdUri).raiseError[F, ProcVisitOutputs[Par]]
        else {
          Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
            .normalize(p.proc_, ProcVisitInputs(VectorPar(), newEnv, input.knownFree))
            .map { bodyResult =>
              val resultNew = New(
                bindCount = newCount,
                p = bodyResult.par,
                uri = uris,
                injections = env,
                locallyFree = bodyResult.par.locallyFree.from(newCount).map(x => x - newCount)
              )
              ProcVisitOutputs(input.par.prepend(resultNew), bodyResult.knownFree)
            }
        }

      }
    }
}
