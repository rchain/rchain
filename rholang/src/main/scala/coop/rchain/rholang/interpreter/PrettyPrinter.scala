package coop.rchain.rholang.interpreter

import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import implicits._
import scalapb.GeneratedMessage
import coop.rchain.shared.StringOps._

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0, "INVALID", "a", 23, 128)

  def apply(i: Int, j: Int): PrettyPrinter = PrettyPrinter(i, j, "INVALID", "a", 23, 128)
}

case class PrettyPrinter(freeShift: Int,
                         boundShift: Int,
                         freeId: String,
                         baseId: String,
                         rotation: Int,
                         maxVarCount: Int) {

  def boundId: String     = rotate(baseId)
  def setBaseId(): String = increment(baseId)

  def buildString(e: Expr): String =
    e.exprInstance match {
      case ENegBody(ENeg(p)) => "-" + buildString(p.get).wrapWithBraces
      case ENotBody(ENot(p)) => "~" + buildString(p.get).wrapWithBraces
      case EMultBody(EMult(p1, p2)) =>
        (buildString(p1.get) + " * " + buildString(p2.get)).wrapWithBraces
      case EDivBody(EDiv(p1, p2)) =>
        (buildString(p1.get) + " / " + buildString(p2.get)).wrapWithBraces
      case EPlusBody(EPlus(p1, p2)) =>
        (buildString(p1.get) + " + " + buildString(p2.get)).wrapWithBraces
      case EMinusBody(EMinus(p1, p2)) =>
        (buildString(p1.get) + " - " + buildString(p2.get)).wrapWithBraces
      case EAndBody(EAnd(p1, p2)) =>
        (buildString(p1.get) + " && " + buildString(p2.get)).wrapWithBraces
      case EOrBody(EOr(p1, p2)) =>
        (buildString(p1.get) + " || " + buildString(p2.get)).wrapWithBraces
      case EEqBody(EEq(p1, p2)) =>
        (buildString(p1.get) + " == " + buildString(p2.get)).wrapWithBraces
      case ENeqBody(ENeq(p1, p2)) =>
        (buildString(p1.get) + " != " + buildString(p2.get)).wrapWithBraces
      case EGtBody(EGt(p1, p2)) =>
        (buildString(p1.get) + " > " + buildString(p2.get)).wrapWithBraces
      case EGteBody(EGte(p1, p2)) =>
        (buildString(p1.get) + " >= " + buildString(p2.get)).wrapWithBraces
      case ELtBody(ELt(p1, p2)) =>
        (buildString(p1.get) + " < " + buildString(p2.get)).wrapWithBraces
      case ELteBody(ELte(p1, p2)) =>
        (buildString(p1.get) + " <= " + buildString(p2.get)).wrapWithBraces
      case EListBody(EList(s, _, _, _)) =>
        "[" + buildSeq(s) + "]"
      case ETupleBody(ETuple(s, _, _)) =>
        "(" + buildSeq(s) + ")"
      case ESetBody(ESet(s, _, _)) =>
        "(" + buildSeq(s) + ")"
      case EMapBody(EMap(kvs, _, _)) =>
        "{" + ("" /: kvs.zipWithIndex) {
          case (string, (kv, i)) =>
            string + buildString(kv.key.get) + " : " + buildString(kv.value.get) + {
              if (i != kvs.length - 1) ", "
              else ""
            }
        } + "}"

      case EVarBody(EVar(v)) => buildString(v.get)
      case EEvalBody(chan)   => "*" + buildString(chan)
      case GBool(b)          => b.toString
      case GInt(i)           => i.toString
      case GString(s)        => "\"" + s + "\""
      case GUri(u)           => s"`$u`"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => "Nil"
      case EMethodBody(method) =>
        "(" + buildString(method.target.get) + ")." + method.methodName + "(" + method.arguments
          .map(buildString)
          .mkString(",") + ")"
      case ExprInstance.GByteArray(bs) => Base16.encode(bs.toByteArray)
      case _                           => throw new Error(s"Attempted to print unknown Expr type: $e")
    }

  def buildString(v: Var): String =
    v.varInstance match {
      case FreeVar(level)  => s"$freeId${freeShift + level}"
      case BoundVar(level) => s"$boundId${boundShift - level - 1}"
      case Wildcard(_)     => "_"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case VarInstance.Empty => "@Nil"
    }

  def buildString(c: Channel): String =
    c.channelInstance match {
      case Quote(p)    => "@{" + buildString(p) + "}"
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
          case ((previousFree, string), (bind, i)) =>
            val bindString =
              this
                .copy(
                  freeShift = boundShift + previousFree,
                  boundShift = 0,
                  freeId = boundId,
                  baseId = setBaseId()
                )
                .buildPattern(bind.patterns)
            (bind.freeCount + previousFree, string + bindString + {
              if (r.persistent) " <= " else " <- "
            } + buildString(bind.source.get) + {
              if (i != r.binds.length - 1) " ; "
              else ""
            })
        }

        "for( " + bindsString + " ) { " + this
          .copy(boundShift = boundShift + totalFree)
          .buildString(r.body.get) + " }"

      case b: Bundle =>
        BundleOps.showInstance.show(b) + "{ " + buildString(b.body.get) + " }"

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
            List(par.bundles, par.sends, par.receives, par.news, par.exprs, par.matches, par.ids)
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

      case _ => throw new Error("Attempt to print unknown GeneratedMessage type.")
    }

  def increment(id: String): String = {
    def incChar(charId: Char): Char = ((charId + 1 - 97) % 26 + 97).toChar

    val newId = incChar(id.last).toString
    if (newId equals "a")
      if (id.length > 1) increment(id.dropRight(1)) + newId
      else "aa"
    else id.dropRight(1) + newId
  }

  def rotate(id: String): String =
    id.map(char => ((char + rotation - 97) % 26 + 97).toChar)

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

  private def buildPattern(patterns: Seq[Channel]): String =
    ("" /: patterns.zipWithIndex) {
      case (string, (pattern, i)) =>
        string + buildString(pattern) + {
          if (i != patterns.length - 1) ", "
          else ""
        }
    }

  private def buildMatchCase(matchCase: MatchCase): String = {
    val patternFree: Int = matchCase.freeCount
    this
      .copy(
        freeShift = boundShift,
        boundShift = 0,
        freeId = boundId,
        baseId = setBaseId()
      )
      .buildString(matchCase.pattern.get) + " => " +
      this
        .copy(boundShift = boundShift + patternFree)
        .buildString(matchCase.source.get)
  }

  private def isEmpty(p: Par) =
    p.sends.isEmpty &
      p.receives.isEmpty &
      p.news.isEmpty &
      p.exprs.isEmpty &
      p.matches.isEmpty &
      p.ids.isEmpty &
      p.bundles.isEmpty
}
