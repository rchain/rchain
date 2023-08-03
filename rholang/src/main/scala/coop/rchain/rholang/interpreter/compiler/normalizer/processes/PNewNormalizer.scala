package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangn.Bindings._
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{NameDeclSimpl, NameDeclUrn, PNew}
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.normalizer.GroundNormalizeMatcher
import coop.rchain.rholang.interpreter.compiler.{
  NameSort,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}

import scala.jdk.CollectionConverters._

object PNewNormalizer {
  def normalize[F[_]: Sync](p: PNew, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {

    // TODO: bindings within a single new shouldn't have overlapping names.
    val newTaggedBindings = p.listnamedecl_.asScala.toVector.map {
      case n: NameDeclSimpl => (None, n.var_, NameSort, n.line_num, n.col_num)
      case n: NameDeclUrn =>
        (
          Some(GroundNormalizeMatcher.stripUri(n.uriliteral_)),
          n.var_,
          NameSort,
          n.line_num,
          n.col_num
        )
    }
    // This sorts the None's first, and the uris by lexicographical order.
    // We do this here because the sorting affects the numbering of variables inside the body.
    val sortBindings = newTaggedBindings.sortBy(row => row._1)
    val newBindings  = sortBindings.map(row => (row._2, row._3, SourcePosition(row._4, row._5)))
    val uris         = sortBindings.flatMap(row => row._1)
    val newEnv       = input.boundMapChain.put(newBindings.toList)
    val newCount     = newEnv.count - input.boundMapChain.count

    normalizeMatch[F](p.proc_, ProcVisitInputs(NilN, newEnv, input.freeMap)).map { bodyResult =>
      val resultNew = NewN(
        bindCount = newCount,
        p = bodyResult.par,
        uri = uris,
        injections = env.map { case (s, par) => (s, fromProto(par)) }
      )
      ProcVisitOutputs(input.par.combine(resultNew), bodyResult.freeMap)
    }

  }
}
