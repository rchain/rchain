package coop.rchain.rholang.interpreter

import coop.rchain.crypto.codec.Base16
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
import coop.rchain.models.GUnforgeable.UnfInstance.{
  GDeployIdBody,
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}
import coop.rchain.shared.Printer
import monix.eval.Coeval

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0)

  def apply(freeShift: Int, boundShift: Int): PrettyPrinter =
    PrettyPrinter(freeShift, boundShift, Vector.empty[Int], "free", "a", 23, 128)

  implicit class CappedOps(private val str: String) extends AnyVal {
    def cap() = Printer.OUTPUT_CAPPED.map(n => s"${str.take(n)}...").getOrElse(str)
  }
}
final case class PrettyPrinter(
    freeShift: Int,
    boundShift: Int,
    newsShiftIndices: Vector[Int],
    freeId: String,
    baseId: String,
    rotation: Int,
    maxVarCount: Int,
    isBuildingChannel: Boolean = false
) {

  val indentStr = "  "

  def boundId: String     = rotate(baseId)
  def setBaseId(): String = increment(baseId)

  import Coeval.pure
  import PrettyPrinter._

  def buildString(e: Expr): String             = buildStringM(e).value.cap()
  def buildString(v: Var): String              = buildStringM(v).value.cap()
  def buildString(m: GeneratedMessage): String = buildStringM(m).value.cap()
  def buildChannelString(p: Par): String       = buildChannelStringM(p).value.cap()

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(u: GUnforgeable): Coeval[String] = Coeval.defer {
    u.unfInstance match {
      case GPrivateBody(p) => pure("Unforgeable(0x" + Base16.encode(p.id.toByteArray) + ")")
      case GDeployIdBody(id) =>
        pure("DeployId(0x" + Base16.encode(id.sig.toByteArray) + ")")
      case GDeployerIdBody(id) =>
        pure("DeployerId(0x" + Base16.encode(id.publicKey.toByteArray) + ")")
      case GSysAuthTokenBody(value) =>
        pure(s"GSysAuthTokenBody(${value})")
      case _ => throw new Error(s"Attempted to print unknown GUnforgeable type: $u")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(e: Expr): Coeval[String] = Coeval.defer {
    e.exprInstance match {

      case ENegBody(ENeg(p)) => pure("-") |+| buildStringM(p).map(_.wrapWithBraces)
      case ENotBody(ENot(p)) => pure("~") |+| buildStringM(p).map(_.wrapWithBraces)
      case EMultBody(EMult(p1, p2)) =>
        (buildStringM(p1) |+| pure(" * ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EDivBody(EDiv(p1, p2)) =>
        (buildStringM(p1) |+| pure(" / ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EModBody(EMod(p1, p2)) =>
        (buildStringM(p1) |+| pure(" % ") |+| buildStringM(p2)).map(_.wrapWithBraces)
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
        pure("{") |+| ps.sortedList.zipWithIndex.foldLeft(pure("")) {
          case (string, (kv, i)) =>
            string |+| buildStringM(kv._1) |+| pure(" : ") |+| buildStringM(kv._2) |+| pure {
              if (i != ps.sortedList.size - 1) ", "
              else ""
            }
        } |+| buildRemainderString(remainder) |+| pure("}")

      case EVarBody(EVar(v)) => buildStringM(v)
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

  private def buildStringM(v: Var): Coeval[String] =
    v.varInstance match {
      case FreeVar(level) => pure(s"$freeId${freeShift + level}")
      case BoundVar(level) =>
        (if (isNewVar(level) && !isBuildingChannel) pure("*") else pure("")) |+| pure(
          s"$boundId${boundShift - level - 1}"
        )
      case Wildcard(_)       => pure("_")
      case VarInstance.Empty => pure("@Nil")
    }

  private def buildChannelStringM(p: Par): Coeval[String] = buildChannelStringM(p, 0)

  private def buildChannelStringM(p: Par, indent: Int): Coeval[String] = {
    def quoteIfNotNew(s: String): String = {
      val isBoundNew = p match {
        case Par(_, _, _, Seq(Expr(EVarBody(EVar(Var(BoundVar(level)))))), _, _, _, _, _, _) =>
          isNewVar(level)
        case _ => false
      }
      if (isBoundNew) s else "@{" + s + "}"
    }

    this.copy(isBuildingChannel = true).buildStringM(p, indent).map { b =>
      if (b.length > 60)
        quoteIfNotNew(b)
      else {
        val whitespace = "\n(\\s\\s)*"
        quoteIfNotNew(b.replaceAll(whitespace, " "))
      }
    }
  }

  private def buildStringM(t: GeneratedMessage): Coeval[String] = buildStringM(t, 0)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(t: GeneratedMessage, indent: Int): Coeval[String] = Coeval.defer {
    val content = t match {
      case v: Var => buildStringM(v)
      case s: Send =>
        buildChannelStringM(s.chan) |+| pure {
          if (s.persistent) "!!("
          else "!("
        } |+| buildSeq(s.data) |+| pure(")")

      case r: Receive =>
        val (totalFree, bindsString) = r.binds.zipWithIndex.foldLeft((0, pure(""))) {
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
              if (r.persistent) " <= " else if (r.peek) " <<- " else " <- "
            } |+| buildChannelStringM(bind.source, indent) |+| pure {
              if (i != r.binds.length - 1) "  & "
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
        val introducedNewsShiftIdx = (0 until n.bindCount).map(i => i + boundShift)
        pure("new " + buildVariables(n.bindCount) + " in {\n" + indentStr * (indent + 1)) |+| this
          .copy(
            boundShift = boundShift + n.bindCount,
            newsShiftIndices = newsShiftIndices ++ introducedNewsShiftIdx
          )
          .buildStringM(n.p, indent + 1) |+|
          pure("\n" + (indentStr * indent) + "}")

      case e: Expr =>
        buildStringM(e)

      case m: Match =>
        pure("match ") |+| buildStringM(m.target) |+| pure(" {\n") |+|
          m.cases.zipWithIndex.foldLeft(pure("")) {
            case (string, (matchCase, i)) =>
              string |+| pure(indentStr * (indent + 1)) |+| buildMatchCase(matchCase, indent + 1) |+| pure {
                if (i != m.cases.length - 1) "\n"
                else ""
              }
          } |+| pure("\n" + (indentStr * indent) + "}")

      case u: GUnforgeable => buildStringM(u)
      case c: Connective =>
        c.connectiveInstance match {
          case ConnectiveInstance.Empty => pure("")
          case ConnAndBody(value) =>
            pure("{") |+| value.ps.map(buildStringM).toList.intercalate(pure(" /\\ ")) |+| pure("}")
          case ConnOrBody(value) =>
            pure("{") |+| value.ps.map(buildStringM).toList.intercalate(pure(" \\/ ")) |+| pure("}")
          case ConnNotBody(value) => pure("~{") |+| buildStringM(value) |+| pure("}")
          case VarRefBody(value)  => pure(s"=$freeId${freeShift - value.index - 1}")
          case _: ConnBool        => pure("Bool")
          case _: ConnInt         => pure("Int")
          case _: ConnString      => pure("String")
          case _: ConnUri         => pure("Uri")
          case _: ConnByteArray   => pure("ByteArray")
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
              par.unforgeables,
              par.connectives
            )
          list.foldLeft((false, pure(""))) {
            // format: off
            case ((prevNonEmpty, string), items) =>
              if (items.nonEmpty) {
                (true,
                 string |+| pure { if (prevNonEmpty) " |\n" + (indentStr * indent) else "" } |+|
                    items.zipWithIndex.foldLeft(pure("")) {
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
    s.zipWithIndex.foldLeft(pure("")) {
      case (string, (p, i)) =>
        string |+| buildStringM(p) |+| pure {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  private def buildPattern(patterns: Seq[Par]): Coeval[String] =
    patterns.zipWithIndex.foldLeft(pure("")) {

      case (string, (pattern, i)) =>
        string |+| buildChannelStringM(pattern) |+| pure {
          if (i != patterns.length - 1) ", "
          else ""
        }
    }

  private def buildMatchCase(matchCase: MatchCase, indent: Int): Coeval[String] = {
    val patternFree: Int = matchCase.freeCount
    val openBrace        = pure(s"{\n${indentStr * (indent + 1)}")
    val closeBrace       = pure(s"\n${indentStr * indent}}")
    this
      .copy(
        freeShift = boundShift,
        boundShift = 0,
        freeId = boundId,
        baseId = setBaseId()
      )
      .buildStringM(matchCase.pattern, indent) |+| pure(" => ") |+| openBrace |+|
      this
        .copy(boundShift = boundShift + patternFree)
        .buildStringM(matchCase.source, indent + 1) |+|
      closeBrace
  }

  private def isEmpty(p: Par) =
    p.sends.isEmpty &
      p.receives.isEmpty &
      p.news.isEmpty &
      p.exprs.isEmpty &
      p.matches.isEmpty &
      p.unforgeables.isEmpty &
      p.bundles.isEmpty &
      p.connectives.isEmpty

  private def isNewVar(level: Int): Boolean = newsShiftIndices.contains(boundShift - level - 1)
}
