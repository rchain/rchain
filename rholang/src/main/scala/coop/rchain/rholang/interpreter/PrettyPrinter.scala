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
import cats.implicits._
import monix.eval.Coeval

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0)

  def apply(freeShift: Int, boundShift: Int): PrettyPrinter =
    PrettyPrinter(freeShift, boundShift, "free", "a", 23, 128)
}

case class PrettyPrinter(
    freeShift: Int,
    boundShift: Int,
    freeId: String,
    baseId: String,
    rotation: Int,
    maxVarCount: Int
) {

  val indentStr = "  "

  def boundId: String     = rotate(baseId)
  def setBaseId(): String = increment(baseId)

  import Coeval.pure

  def buildString(e: Expr): String             = buildStringM(e).value
  def buildString(v: Var): String              = buildStringM(v).value
  def buildString(c: Channel): String          = buildStringM(c).value
  def buildString(m: GeneratedMessage): String = buildStringM(m).value

  private def buildStringM(e: Expr): Coeval[String] = Coeval.defer {
    e.exprInstance match {

      case ENegBody(ENeg(p)) => pure("-") |+| buildStringM(p).map(_.wrapWithBraces)
      case ENotBody(ENot(p)) => pure("~") |+| buildStringM(p).map(_.wrapWithBraces)
      case EMultBody(EMult(p1, p2)) =>
        (buildStringM(p1) |+| pure(" * ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EDivBody(EDiv(p1, p2)) =>
        (buildStringM(p1) |+| pure(" / ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPercentPercentBody(EPercentPercent(p1, p2)) =>
        (buildStringM(p1) |+| pure(" %% ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPlusBody(EPlus(p1, p2)) =>
        (buildStringM(p1) |+| pure(" + ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPlusPlusBody(EPlusPlus(p1, p2)) =>
        (buildStringM(p1) |+| pure(" ++ ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMinusBody(EMinus(p1, p2)) =>
        (buildStringM(p1) |+| pure(" - ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMinusMinusBody(EMinusMinus(p1, p2)) =>
        (buildStringM(p1) |+| pure(" -- ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EAndBody(EAnd(p1, p2)) =>
        (buildStringM(p1) |+| pure(" && ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EOrBody(EOr(p1, p2)) =>
        (buildStringM(p1) |+| pure(" || ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EEqBody(EEq(p1, p2)) =>
        (buildStringM(p1) |+| pure(" == ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ENeqBody(ENeq(p1, p2)) =>
        (buildStringM(p1) |+| pure(" != ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EGtBody(EGt(p1, p2)) =>
        (buildStringM(p1) |+| pure(" > ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EGteBody(EGte(p1, p2)) =>
        (buildStringM(p1) |+| pure(" >= ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ELtBody(ELt(p1, p2)) =>
        (buildStringM(p1) |+| pure(" < ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ELteBody(ELte(p1, p2)) =>
        (buildStringM(p1) |+| pure(" <= ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMatchesBody(EMatches(target, pattern)) =>
        (buildStringM(target) |+| pure(" matches ") |+| buildStringM(pattern))
          .map(_.wrapWithBraces)
      case EListBody(EList(s, _, _, remainder)) =>
        pure("[") |+| buildSeq(s) |+| buildRemainderString(remainder) |+| pure("]")
      case ETupleBody(ETuple(s, _, _)) =>
        pure("(") |+| buildSeq(s) |+| pure(")")
      case ESetBody(ParSet(pars, _, _, remainder)) =>
        pure("Set(") |+| buildSeq(pars.sortedPars) |+| buildRemainderString(remainder) |+| pure(")")
      case EMapBody(ParMap(ps, _, _, remainder)) =>
        pure("{") |+| (pure("") /: ps.sortedMap.zipWithIndex) {
          case (string, (kv, i)) =>
            string |+| buildStringM(kv._1) |+| pure(" : ") |+| buildStringM(kv._2) |+| pure {
              if (i != ps.sortedMap.length - 1) ", "
              else ""
            }
        } |+| buildRemainderString(remainder) |+| pure("}")

      case EVarBody(EVar(v)) => buildStringM(v)
      case EEvalBody(chan)   => pure("*") |+| buildStringM(chan)
      case GBool(b)          => pure(b.toString)
      case GInt(i)           => pure(i.toString)
      case GString(s)        => pure("\"" + s + "\"")
      case GUri(u)           => pure(s"`$u`")
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => pure("Nil")
      case EMethodBody(method) =>
        val args = method.arguments.map(buildStringM).toList.intercalate(pure(","))
        pure("(") |+| buildStringM(method.target) |+| pure(")." + method.methodName + "(") |+| args |+| pure(
          ")"
        )
      case ExprInstance.GByteArray(bs) => pure(Base16.encode(bs.toByteArray))
      case _                           => throw new Error(s"Attempted to print unknown Expr type: $e")
    }
  }

  private def buildRemainderString(remainder: Option[Var]): Coeval[String] =
    remainder.fold(pure(""))(v => pure("...") |+| buildStringM(v))

  def buildStringM(v: Var): Coeval[String] =
    v.varInstance match {
      case FreeVar(level)    => pure(s"$freeId${freeShift + level}")
      case BoundVar(level)   => pure(s"$boundId${boundShift - level - 1}")
      case Wildcard(_)       => pure("_")
      case VarInstance.Empty => pure("@Nil")
    }

  def buildStringM(c: Channel): Coeval[String] =
    c.channelInstance match {
      case Quote(p) =>
        buildStringM(p).map { b =>
          if (b.size > 60) {
            "@{" + b + "}"
          } else {
            "@{" + b.replaceAll("[\n](\\s\\s)*", (" ")) + "}"
          }
        }
      case ChanVar(cv)           => buildStringM(cv)
      case ChannelInstance.Empty => pure("@Nil")
    }

  def buildStringM(t: GeneratedMessage): Coeval[String] = buildStringM(t, 0)

  def buildStringM(t: GeneratedMessage, indent: Int): Coeval[String] = Coeval.defer {
    val content = t match {
      case v: Var     => buildStringM(v)
      case c: Channel => buildStringM(c)
      case s: Send =>
        buildStringM(s.chan) |+| pure {
          if (s.persistent) "!!("
          else "!("
        } |+| buildSeq(s.data) |+| pure(")")

      case r: Receive =>
        val (totalFree, bindsString) = ((0, pure("")) /: r.binds.zipWithIndex) {
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
            (bind.freeCount + previousFree, string |+| bindString |+| pure {
              if (r.persistent) " <= " else " <- "
            } |+| buildStringM(bind.source, indent) |+| pure {
              if (i != r.binds.length - 1) " ; "
              else ""
            })
        }

        this
          .copy(boundShift = boundShift + totalFree)
          .buildStringM(r.body, indent + 1)
          .flatMap { bodyStr =>
            if (bodyStr.nonEmpty) {
              pure("for( ") |+| bindsString |+| pure(
                " ) {\n" + (indentStr * (indent + 1)) + bodyStr + "\n" + (indentStr * indent) + "}"
              )
            } else {
              pure("for( ") |+| bindsString |+| pure(" ) {" + bodyStr + "}")
            }
          }

      case b: Bundle =>
        pure(BundleOps.showInstance.show(b) + "{ " + (indentStr * (indent + 1))) |+|
          buildStringM(b.body, indent + 1) |+| pure(" }")

      case n: New =>
        pure("new " + buildVariables(n.bindCount) + " in {\n" + indentStr * (indent + 1)) |+| this
          .copy(boundShift = boundShift + n.bindCount)
          .buildStringM(n.p, indent + 1) |+|
          pure("\n" + (indentStr * indent) + "}")

      case e: Expr =>
        buildStringM(e)

      case m: Match =>
        pure("match ") |+| buildStringM(m.target) |+| pure(" {\n") |+|
          (pure("") /: m.cases.zipWithIndex) {
            case (string, (matchCase, i)) =>
              string |+| pure(indentStr * (indent + 1)) |+| buildMatchCase(matchCase, indent + 1) |+| pure {
                if (i != m.cases.length - 1) " ;\n"
                else ""
              }
          } |+| pure("\n" + (indentStr * indent) + "}")

      case g: GPrivate => pure("Unforgeable(0x" + Base16.encode(g.id.toByteArray) + ")")
      case c: Connective =>
        c.connectiveInstance match {
          case ConnectiveInstance.Empty => pure("")
          case ConnAndBody(value) =>
            pure("{") |+| value.ps.map(buildStringM).toList.intercalate(pure(" /\\ ")) |+| pure("}")
          case ConnOrBody(value) =>
            pure("{") |+| value.ps.map(buildStringM).toList.intercalate(pure(" \\/ ")) |+| pure("}")
          case ConnNotBody(value) => pure("~{") |+| buildStringM(value) |+| pure("}")
          case VarRefBody(value) =>
            pure("=") |+| buildStringM(Var(FreeVar(value.index)))
          case _: ConnBool      => pure("Bool")
          case _: ConnInt       => pure("Int")
          case _: ConnString    => pure("String")
          case _: ConnUri       => pure("Uri")
          case _: ConnByteArray => pure("ByteArray")
        }

      case par: Par =>
        if (isEmpty(par)) pure("Nil")
        else {
          val list =
            List(
              par.bundles,
              par.sends,
              par.receives,
              par.news,
              par.exprs,
              par.matches,
              par.ids,
              par.connectives
            )
          ((false, pure("")) /: list) {
            // format: off
            case ((prevNonEmpty, string), items) =>
              if (items.nonEmpty) {
                (true,
                 string |+| pure { if (prevNonEmpty) " |\n" + (indentStr * indent) else "" } |+|
                   (pure("") /: items.zipWithIndex) {
                     case (_string, (_par, index)) =>
                       _string |+| buildStringM(_par, indent) |+| pure {
                         if (index != items.length - 1) " |\n" + (indentStr * indent) else ""
                       }
                   })
              } else (prevNonEmpty, string)
            // format: on
          }
        }._2

      case unsupported =>
        throw new Error(s"Attempt to print unknown GeneratedMessage type: ${unsupported.getClass}.")
    }
    content
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

  private def buildSeq[T <: GeneratedMessage](s: Seq[T]): Coeval[String] =
    (pure("") /: s.zipWithIndex) {
      case (string, (p, i)) =>
        string |+| buildStringM(p) |+| pure {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  private def buildPattern(patterns: Seq[Channel]): Coeval[String] =
    (pure("") /: patterns.zipWithIndex) {

      case (string, (pattern, i)) =>
        string |+| buildStringM(pattern) |+| pure {
          if (i != patterns.length - 1) ", "
          else ""
        }
    }

  private def buildMatchCase(matchCase: MatchCase, indent: Int): Coeval[String] = {
    val patternFree: Int = matchCase.freeCount
    this
      .copy(
        freeShift = boundShift,
        boundShift = 0,
        freeId = boundId,
        baseId = setBaseId()
      )
      .buildStringM(matchCase.pattern, indent) |+| pure(" => ") |+|
      this
        .copy(boundShift = boundShift + patternFree)
        .buildStringM(matchCase.source, indent)
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
