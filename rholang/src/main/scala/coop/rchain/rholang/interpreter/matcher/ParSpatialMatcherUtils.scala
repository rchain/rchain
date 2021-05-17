package coop.rchain.rholang.interpreter.matcher

import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.Var.VarInstance.{FreeVar, Wildcard}
import coop.rchain.models.{EVar, Expr, Par}

private[matcher] object ParSpatialMatcherUtils {

  def noFrees(par: Par): Par =
    par.withExprs(noFrees(par.exprs))

  def noFrees(exprs: Vector[Expr]): Vector[Expr] =
    exprs.filter({ (expr) =>
      expr.exprInstance match {
        case EVarBody(EVar(v)) =>
          v.varInstance match {
            case FreeVar(_)  => false
            case Wildcard(_) => false
            case _           => true
          }
        case _ => true
      }
    })

  def subPars(
      par: Par,
      min: ParCount,
      max: ParCount,
      minPrune: ParCount,
      maxPrune: ParCount
  ): Stream[(Par, Par)] = {

    val sendMax    = math.min(max.sends, par.sends.size - minPrune.sends)
    val receiveMax = math.min(max.receives, par.receives.size - minPrune.receives)
    val newMax     = math.min(max.news, par.news.size - minPrune.news)
    val exprMax    = math.min(max.exprs, par.exprs.size - minPrune.exprs)
    val matchMax   = math.min(max.matches, par.matches.size - minPrune.matches)
    val unfMax     = math.min(max.unforgeables, par.unforgeables.size - minPrune.unforgeables)
    val bundleMax  = math.min(max.bundles, par.bundles.size - minPrune.bundles)

    val sendMin    = math.max(min.sends, par.sends.size - maxPrune.sends)
    val receiveMin = math.max(min.receives, par.receives.size - maxPrune.receives)
    val newMin     = math.max(min.news, par.news.size - maxPrune.news)
    val exprMin    = math.max(min.exprs, par.exprs.size - maxPrune.exprs)
    val matchMin   = math.max(min.matches, par.matches.size - maxPrune.matches)
    val unfMin     = math.max(min.unforgeables, par.unforgeables.size - maxPrune.unforgeables)
    val bundleMin  = math.max(min.bundles, par.bundles.size - maxPrune.bundles)

    for {
      subSends    <- minMaxSubsets(par.sends, sendMin, sendMax)
      subReceives <- minMaxSubsets(par.receives, receiveMin, receiveMax)
      subNews     <- minMaxSubsets(par.news, newMin, newMax)
      subExprs    <- minMaxSubsets(par.exprs, exprMin, exprMax)
      subMatches  <- minMaxSubsets(par.matches, matchMin, matchMax)
      subUnfs     <- minMaxSubsets(par.unforgeables, unfMin, unfMax)
      subBundles  <- minMaxSubsets(par.bundles, bundleMin, bundleMax)
    } yield (
      Par(
        subSends._1,
        subReceives._1,
        subNews._1,
        subExprs._1,
        subMatches._1,
        subUnfs._1,
        subBundles._1
      ),
      Par(
        subSends._2,
        subReceives._2,
        subNews._2,
        subExprs._2,
        subMatches._2,
        subUnfs._2,
        subBundles._2
      )
    )
  }

  def minMaxSubsets[A](
      as: Vector[A],
      minSize: Int,
      maxSize: Int
  ): Stream[(Vector[A], Vector[A])] = {

    def countedMaxSubsets(as: Vector[A], maxSize: Int): Stream[(Vector[A], Vector[A], Int)] =
      as match {
        case Vector() => Stream((as, as, 0))
        case head +: rem =>
          (as.slice(0, 0), as, 0) #::
            (for {
              countedTail               <- countedMaxSubsets(rem, maxSize)
              (tail, complement, count) = countedTail
              result <- {
                if (count == maxSize)
                  Stream((tail, head +: complement, count))
                else if (tail.isEmpty)
                  Stream((head +: tail, complement, 1))
                else
                  Stream((tail, head +: complement, count), (head +: tail, complement, count + 1))
              }
            } yield result)
      }

    def worker(as: Vector[A], minSize: Int, maxSize: Int): Stream[(Vector[A], Vector[A], Int)] =
      if (maxSize < 0)
        Stream.empty
      else if (minSize > maxSize)
        Stream.empty
      else if (minSize <= 0)
        if (maxSize == 0)
          Stream((as.slice(0, 0), as, 0))
        else
          countedMaxSubsets(as, maxSize)
      else
        //TODO the below is duplicated in countedMaxSubsets. Deduplicate.
        as match {
          case Vector() => Stream.empty
          case head +: rem =>
            val decr = minSize - 1
            for {
              countedTail               <- worker(rem, decr, maxSize)
              (tail, complement, count) = countedTail
              result <- {
                if (count == maxSize)
                  Stream((tail, head +: complement, count))
                else if (count == decr)
                  Stream((head +: tail, complement, minSize))
                else
                  Stream((tail, head +: complement, count), (head +: tail, complement, count + 1))
              }
            } yield result
        }

    worker(as, minSize, maxSize).map(x => (x._1, x._2))
  }

}
