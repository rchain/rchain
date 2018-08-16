package coop.rchain.rholang.interpreter

import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import scalapb.GeneratedMessage
import coop.rchain.shared.StringOps._

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0)

  def apply(freeShift: Int, boundShift: Int): PrettyPrinter =
    PrettyPrinter(freeShift, boundShift, "free", "a", 23, 128)
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
      case ENegBody(ENeg(p)) => "-" + buildString(p).wrapWithBraces
      case ENotBody(ENot(p)) => "~" + buildString(p).wrapWithBraces
      case EMultBody(EMult(p1, p2)) =>
        (buildString(p1) + " * " + buildString(p2)).wrapWithBraces
      case EDivBody(EDiv(p1, p2)) =>
        (buildString(p1) + " / " + buildString(p2)).wrapWithBraces
      case EPercentPercentBody(EPercentPercent(p1, p2)) =>
        (buildString(p1) + " %% " + buildString(p2)).wrapWithBraces
      case EPlusBody(EPlus(p1, p2)) =>
        (buildString(p1) + " + " + buildString(p2)).wrapWithBraces
      case EPlusPlusBody(EPlusPlus(p1, p2)) =>
        (buildString(p1) + " ++ " + buildString(p2)).wrapWithBraces
      case EMinusBody(EMinus(p1, p2)) =>
        (buildString(p1) + " - " + buildString(p2)).wrapWithBraces
      case EMinusMinusBody(EMinusMinus(p1, p2)) =>
        (buildString(p1) + " -- " + buildString(p2)).wrapWithBraces
      case EAndBody(EAnd(p1, p2)) =>
        (buildString(p1) + " && " + buildString(p2)).wrapWithBraces
      case EOrBody(EOr(p1, p2)) =>
        (buildString(p1) + " || " + buildString(p2)).wrapWithBraces
      case EEqBody(EEq(p1, p2)) =>
        (buildString(p1) + " == " + buildString(p2)).wrapWithBraces
      case ENeqBody(ENeq(p1, p2)) =>
        (buildString(p1) + " != " + buildString(p2)).wrapWithBraces
      case EGtBody(EGt(p1, p2)) =>
        (buildString(p1) + " > " + buildString(p2)).wrapWithBraces
      case EGteBody(EGte(p1, p2)) =>
        (buildString(p1) + " >= " + buildString(p2)).wrapWithBraces
      case ELtBody(ELt(p1, p2)) =>
        (buildString(p1) + " < " + buildString(p2)).wrapWithBraces
      case ELteBody(ELte(p1, p2)) =>
        (buildString(p1) + " <= " + buildString(p2)).wrapWithBraces
      case EMatchesBody(EMatches(target, pattern)) =>
        (buildString(target) + " matches " + buildString(pattern)).wrapWithBraces
      case EListBody(EList(s, _, _, remainder)) =>
        "[" + buildSeq(s) + buildRemainderString(remainder) + "]"
      case ETupleBody(ETuple(s, _, _)) =>
        "(" + buildSeq(s) + ")"
      case ESetBody(ParSet(pars, _, _)) =>
        "Set(" + buildSeq(pars.sortedPars.toSeq) + ")"
      case EMapBody(ParMap(ps, _, _)) =>
        "{" + ("" /: ps.sortedMap.zipWithIndex) {
          case (string, (kv, i)) =>
            string + buildString(kv._1) + " : " + buildString(kv._2) + {
              if (i != ps.sortedMap.length - 1) ", "
              else ""
            }
        } + "}"

      case EVarBody(EVar(v)) => buildString(v)
      case EEvalBody(chan)   => "*" + buildString(chan)
      case GBool(b)          => b.toString
      case GInt(i)           => i.toString
      case GString(s)        => "\"" + s + "\""
      case GUri(u)           => s"`$u`"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => "Nil"
      case EMethodBody(method) =>
        "(" + buildString(method.target) + ")." + method.methodName + "(" + method.arguments
          .map(buildString)
          .mkString(",") + ")"
      case ExprInstance.GByteArray(bs) => Base16.encode(bs.toByteArray)
      case _                           => throw new Error(s"Attempted to print unknown Expr type: $e")
    }

  private def buildRemainderString(remainder: Option[Var]): String =
    remainder.fold("")(v => "..." + buildString(v))

  def buildString(v: Var): String =
    v.varInstance match {
      case FreeVar(level)    => s"$freeId${freeShift + level}"
      case BoundVar(level)   => s"$boundId${boundShift - level - 1}"
      case Wildcard(_)       => "_"
      case VarInstance.Empty => "@Nil"
    }

  def buildString(c: Channel): String =
    c.channelInstance match {
      case Quote(p)              => "@{" + buildString(p) + "}"
      case ChanVar(cv)           => buildString(cv)
      case ChannelInstance.Empty => "@Nil"
    }

  def buildString(t: GeneratedMessage): String =
    t match {
      case v: Var     => buildString(v)
      case c: Channel => buildString(c)
      case s: Send =>
        buildString(s.chan) + {
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
            } + buildString(bind.source) + {
              if (i != r.binds.length - 1) " ; "
              else ""
            })
        }

        "for( " + bindsString + " ) { " + this
          .copy(boundShift = boundShift + totalFree)
          .buildString(r.body) + " }"

      case b: Bundle =>
        BundleOps.showInstance.show(b) + "{ " + buildString(b.body) + " }"

      case n: New =>
        "new " + buildVariables(n.bindCount) + " in { " + this
          .copy(boundShift = boundShift + n.bindCount)
          .buildString(n.p) + " }"

      case e: Expr =>
        buildString(e)

      case m: Match =>
        "match " + buildString(m.target) + " { " +
          ("" /: m.cases.zipWithIndex) {
            case (string, (matchCase, i)) =>
              string + buildMatchCase(matchCase) + {
                if (i != m.cases.length - 1) " ; "
                else ""
              }
          } + " }"

      case g: GPrivate => "Unforgeable(0x" + Base16.encode(g.id.toByteArray) + ")"
      case c: Connective =>
        c.connectiveInstance match {
          case ConnectiveInstance.Empty => ""
          case ConnAndBody(value)       => value.ps.map(buildString).mkString("{", " /\\ ", "}")
          case ConnOrBody(value)        => value.ps.map(buildString).mkString("{", " \\/ ", "}")
          case ConnNotBody(value)       => "~{" ++ buildString(value) ++ "}"
          case VarRefBody(value) =>
            "=" + buildString(Var(FreeVar(value.index)))
          case _: ConnBool      => "Bool"
          case _: ConnInt       => "Int"
          case _: ConnString    => "String"
          case _: ConnUri       => "Uri"
          case _: ConnByteArray => "ByteArray"
        }

      case par: Par =>
        if (isEmpty(par)) "Nil"
        else {
          val list =
            List(par.bundles,
                 par.sends,
                 par.receives,
                 par.news,
                 par.exprs,
                 par.matches,
                 par.ids,
                 par.connectives)
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

      case unsupported =>
        throw new Error(s"Attempt to print unknown GeneratedMessage type: ${unsupported.getClass}.")
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
      .buildString(matchCase.pattern) + " => " +
      this
        .copy(boundShift = boundShift + patternFree)
        .buildString(matchCase.source)
  }

  private def isEmpty(p: Par) =
    p.sends.isEmpty &
      p.receives.isEmpty &
      p.news.isEmpty &
      p.exprs.isEmpty &
      p.matches.isEmpty &
      p.ids.isEmpty &
      p.bundles.isEmpty &
      p.connectives.isEmpty
}
