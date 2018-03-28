package coop.rchain.rholang.interpreter

import com.trueaccord.scalapb.GeneratedMessage
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.implicits.ChannelLocallyFree._

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0, "a", "x", 23, 128)
  def apply(i: Int, j: Int): PrettyPrinter = PrettyPrinter(i, j, "a", "x", 23, 128)
}

case class PrettyPrinter(freeShift: Int,
                         boundShift: Int,
                         freeId: String,
                         boundId: String,
                         rotation: Int,
                         maxVarCount: Int) {

  def buildString(e: Expr): String =
    e.exprInstance match {
      case ENegBody(ENeg(p)) => "-" + buildString(p.get)
      case ENotBody(ENot(p)) => "~" + buildString(p.get)
      case EMultBody(EMult(p1, p2)) =>
        buildString(p1.get) + " * " + buildString(p2.get)
      case EDivBody(EDiv(p1, p2)) =>
        buildString(p1.get) + " / " + buildString(p2.get)
      case EPlusBody(EPlus(p1, p2)) =>
        buildString(p1.get) + " + " + buildString(p2.get)
      case EMinusBody(EMinus(p1, p2)) =>
        buildString(p1.get) + " - " + buildString(p2.get)
      case EAndBody(EAnd(p1, p2)) =>
        buildString(p1.get) + " && " + buildString(p2.get)
      case EOrBody(EOr(p1, p2)) =>
        buildString(p1.get) + " || " + buildString(p2.get)
      case EEqBody(EEq(p1, p2)) =>
        buildString(p1.get) + " == " + buildString(p2.get)
      case ENeqBody(ENeq(p1, p2)) =>
        buildString(p1.get) + " != " + buildString(p2.get)
      case EGtBody(EGt(p1, p2)) =>
        buildString(p1.get) + " > " + buildString(p2.get)
      case EGteBody(EGte(p1, p2)) =>
        buildString(p1.get) + " >= " + buildString(p2.get)
      case ELtBody(ELt(p1, p2)) =>
        buildString(p1.get) + " < " + buildString(p2.get)
      case ELteBody(ELte(p1, p2)) =>
        buildString(p1.get) + " <= " + buildString(p2.get)
      case EListBody(EList(s, _, _, _)) =>
        "[" + buildSeq(s) + "]"
      case ETupleBody(ETuple(s, _, _, _)) =>
        "[" + buildSeq(s) + "]"
      case ESetBody(ESet(s, _, _, _)) =>
        "(" + buildSeq(s) + ")"
      case EMapBody(EMap(kvs, _, _, _)) =>
        "{" + ("" /: kvs.zipWithIndex) {
          case (string, (kv, i)) =>
            string + buildString(kv.key.get) + " : " + buildString(kv.value.get) + {
              if (i != kvs.length - 1) ", "
              else ""
            }
        } + "}"

      case EVarBody(EVar(v)) => buildString(v.get)
      case GBool(b)          => b.toString
      case GInt(i)           => i.toString
      case GString(s)        => "\"" + s + "\""
      case GUri(u)           => s"`$u`"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => "Nil"
      case _                  => throw new Error("Attempted to print unknown Expr type")
    }

  def buildString(v: Var): String =
    v.varInstance match {
      case FreeVar(level) => s"$freeId${freeShift + level}"
      case BoundVar(level) => s"$boundId${boundShift - level - 1}"
      case Wildcard(_)     => "_"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case VarInstance.Empty => "@Nil"
    }

  def buildString(c: Channel): String =
    c.channelInstance match {
      case Quote(p) => "@{" + buildString(p) + "}"
      case ChanVar(cv) => buildString(cv)
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ChannelInstance.Empty => "@Nil"
    }

  def buildString(t: GeneratedMessage): String =
    t match {
      case v: Var     => buildString(v)
      case c: Channel => buildString(c)
      case s: Send =>
        buildString(s.chan.get) + {
          if (s.persistent) "!!("
          else "!("
        } + buildSeq(s.data) + ")"

      case r: Receive =>
        val (totalFree, bindsString) = ((0, "") /: r.binds.zipWithIndex) {
          case ((free, string), (bind, i)) =>
            val (patternFree, bindString) =
              this
                .copy(freeShift = boundShift + free, boundShift = 0)
                .buildPattern(bind.patterns)
            (free + patternFree, string + bindString + {
              if (r.persistent) " <= " else " <- "
            } + buildString(bind.source.get) + {
              if (i != r.binds.length - 1) " ; "
              else ""
            })
        }
        "for( " + bindsString + " ) { " + this
          .copy(boundShift = boundShift + totalFree)
          .buildString(r.body.get) + " }"

      case e: Eval => "*" + buildString(e.channel.get)

      case n: New =>
        "new " + buildVariables(n.bindCount) + " in { " + this
          .copy(boundShift = boundShift + n.bindCount)
          .buildString(n.p.get) + " }"

      case e: Expr =>
        buildString(e)

      case m: Match =>
        "match " + buildString(m.target.get) + " { " +
          ("" /: m.cases.zipWithIndex) {
            case (string, (matchCase, i)) =>
              string + buildMatchCase(matchCase) + {
                if (i != m.cases.length - 1) " ; "
                else ""
              }
          } + " }"

      case g: GPrivate => g.id

      case par: Par =>
        if (isEmpty(par)) "Nil"
        else {
          val list =
            List(par.sends, par.receives, par.evals, par.news, par.exprs, par.matches, par.ids)
          ((false, "") /: list) {
            case ((prevNonEmpty, string), items) =>
              if (items.nonEmpty) {
                (true, string + { if (prevNonEmpty) " | " else "" } + ("" /: items.zipWithIndex) {
                  case (_string, (_par, index)) =>
                    _string + buildString(_par) + {
                      if (index != items.length - 1) " | " else ""
                    }
                })
              } else (prevNonEmpty, string)
          }
        }._2

      case _ => throw new Error("Attempt to print unknown GeneratedMessage type")
    }

  private def buildVariables(bindCount: Int): String =
    (0 until Math.min(maxVarCount, bindCount))
      .map(i => s"$boundId${boundShift + i}")
      .mkString(", ")

  private def buildSeq[T <: GeneratedMessage](s: Seq[T]): String =
    ("" /: s.zipWithIndex) {
      case (string, (p, i)) =>
        string + buildString(p) + {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  private def buildPattern(patterns: Seq[Channel]): (Int, String) =
    ((0, "") /: patterns.zipWithIndex) {
      case ((patternsFree, string), (pattern, i)) =>
        (patternsFree + freeCount(pattern),
          string +
            this
              .copy(freeId = boundId, boundId = rotate(boundId))
              .buildString(pattern) + {
            if (i != patterns.length - 1) ", "
            else ""
          })
    }

  private def buildMatchCase(matchCase: MatchCase): String = {
    val patternFree: Int = matchCase.pattern.get.freeCount
    "case " +
      this
        .copy(freeId = boundId, boundId = rotate(boundId), freeShift = boundShift, boundShift = 0)
        .buildString(matchCase.pattern.get) + " => " +
      this
        .copy(boundShift = boundShift + patternFree)
        .buildString(matchCase.source.get)
  }

  private def rotate(id: String): String = {
    val newId = ((id.last + rotation - 97) % 26 + 97).toChar
    if (newId equals id(0)) id.dropRight(1) ++ newId.toString * 2
    else id.dropRight(1) ++ newId.toString
  }

  private def isEmpty(p: Par) =
    p.sends.isEmpty &
      p.receives.isEmpty &
      p.evals.isEmpty &
      p.news.isEmpty &
      p.exprs.isEmpty &
      p.matches.isEmpty &
      p.ids.isEmpty
}
