package coop.rchain.rholang.interpreter

import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import scalapb.GeneratedMessage
import coop.rchain.shared.StringOps._
import cats.syntax.all._
import coop.rchain.models.GUnforgeable.UnfInstance.{
  GDeployIdBody,
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}
import coop.rchain.shared.{Base16, Printer}
import cats.Eval

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

  import PrettyPrinter._

  def buildString(e: Expr): String             = buildStringM(e).value.cap()
  def buildString(v: Var): String              = buildStringM(v).value.cap()
  def buildString(m: GeneratedMessage): String = buildStringM(m).value.cap()
  def buildChannelString(p: Par): String       = buildChannelStringM(p).value.cap()

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(u: GUnforgeable): Eval[String] = Eval.defer {
    u.unfInstance match {
      case GPrivateBody(p) => Eval.now("Unforgeable(0x" + Base16.encode(p.id.toByteArray) + ")")
      case GDeployIdBody(id) =>
        Eval.now("DeployId(0x" + Base16.encode(id.sig.toByteArray) + ")")
      case GDeployerIdBody(id) =>
        Eval.now("DeployerId(0x" + Base16.encode(id.publicKey.toByteArray) + ")")
      case GSysAuthTokenBody(value) =>
        Eval.now(s"GSysAuthTokenBody(${value})")
      case _ => throw new Error(s"Attempted to print unknown GUnforgeable type: $u")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(e: Expr): Eval[String] = Eval.defer {
    e.exprInstance match {

      case ENegBody(ENeg(p)) => Eval.now("-") |+| buildStringM(p).map(_.wrapWithBraces)
      case ENotBody(ENot(p)) => Eval.now("~") |+| buildStringM(p).map(_.wrapWithBraces)
      case EMultBody(EMult(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" * ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EDivBody(EDiv(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" / ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EModBody(EMod(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" % ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPercentPercentBody(EPercentPercent(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" %% ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPlusBody(EPlus(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" + ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EPlusPlusBody(EPlusPlus(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" ++ ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMinusBody(EMinus(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" - ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMinusMinusBody(EMinusMinus(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" -- ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EAndBody(EAnd(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" and ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EOrBody(EOr(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" or ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EShortAndBody(EShortAnd(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" && ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EShortOrBody(EShortOr(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" || ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EEqBody(EEq(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" == ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ENeqBody(ENeq(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" != ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EGtBody(EGt(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" > ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EGteBody(EGte(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" >= ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ELtBody(ELt(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" < ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case ELteBody(ELte(p1, p2)) =>
        (buildStringM(p1) |+| Eval.now(" <= ") |+| buildStringM(p2)).map(_.wrapWithBraces)
      case EMatchesBody(EMatches(target, pattern)) =>
        (buildStringM(target) |+| Eval.now(" matches ") |+| buildStringM(pattern))
          .map(_.wrapWithBraces)
      case EListBody(EList(s, _, _, remainder)) =>
        Eval.now("[") |+| buildSeq(s) |+| buildRemainderString(remainder) |+| Eval.now("]")
      case ETupleBody(ETuple(s, _, _)) =>
        Eval.now("(") |+| buildSeq(s) |+| Eval.now(")")
      case ESetBody(ParSet(pars, _, _, remainder)) =>
        Eval.now("Set(") |+| buildSeq(pars.sortedPars) |+| buildRemainderString(remainder) |+| Eval
          .now(")")
      case EMapBody(ParMap(ps, _, _, remainder)) =>
        Eval.now("{") |+| ps.sortedList.zipWithIndex.foldLeft(Eval.now("")) {
          case (string, (kv, i)) =>
            string |+| buildStringM(kv._1) |+| Eval.now(" : ") |+| buildStringM(kv._2) |+| Eval
              .now {
                if (i != ps.sortedList.size - 1) ", "
                else ""
              }
        } |+| buildRemainderString(remainder) |+| Eval.now("}")

      case EVarBody(EVar(v)) => buildStringM(v)
      case GBool(b)          => Eval.now(b.toString)
      case GInt(i)           => Eval.now(i.toString)
      case GBigInt(bi)       => Eval.now(s"BigInt($bi)")
      case GString(s)        => Eval.now("\"" + s + "\"")
      case GUri(u)           => Eval.now(s"`$u`")
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => Eval.now("Nil")
      case EMethodBody(method) =>
        val args = method.arguments.map(buildStringM).toList.intercalate(Eval.now(","))
        Eval.now("(") |+| buildStringM(method.target) |+| Eval
          .now(")." + method.methodName + "(") |+| args |+| Eval.now(
          ")"
        )
      case ExprInstance.GByteArray(bs) => Eval.now(Base16.encode(bs.toByteArray))
      case _                           => throw new Error(s"Attempted to print unknown Expr type: $e")
    }
  }

  private def buildRemainderString(remainder: Option[Var]): Eval[String] =
    remainder.fold(Eval.now(""))(v => Eval.now("...") |+| buildStringM(v))

  private def buildStringM(v: Var): Eval[String] =
    v.varInstance match {
      case FreeVar(level) => Eval.now(s"$freeId${freeShift + level}")
      case BoundVar(level) =>
        (if (isNewVar(level) && !isBuildingChannel) Eval.now("*") else Eval.now("")) |+| Eval.now(
          s"$boundId${boundShift - level - 1}"
        )
      case Wildcard(_)       => Eval.now("_")
      case VarInstance.Empty => Eval.now("@Nil")
    }

  private def buildChannelStringM(p: Par): Eval[String] = buildChannelStringM(p, 0)

  private def buildChannelStringM(p: Par, indent: Int): Eval[String] = {
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

  private def buildStringM(t: GeneratedMessage): Eval[String] = buildStringM(t, 0)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def buildStringM(t: GeneratedMessage, indent: Int): Eval[String] = Eval.defer {
    val content = t match {
      case v: Var => buildStringM(v)
      case s: Send =>
        buildChannelStringM(s.chan) |+| Eval.now {
          if (s.persistent) "!!("
          else "!("
        } |+| buildSeq(s.data) |+| Eval.now(")")

      case r: Receive =>
        val (totalFree, bindsString) = r.binds.zipWithIndex.foldLeft((0, Eval.now(""))) {
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
            (bind.freeCount + previousFree, string |+| bindString |+| Eval.now {
              if (r.persistent) " <= " else if (r.peek) " <<- " else " <- "
            } |+| buildChannelStringM(bind.source, indent) |+| Eval.now {
              if (i != r.binds.length - 1) "  & "
              else ""
            })
        }

        this
          .copy(boundShift = boundShift + totalFree)
          .buildStringM(r.body, indent + 1)
          .flatMap { bodyStr =>
            if (bodyStr.nonEmpty) {
              Eval.now("for( ") |+| bindsString |+| Eval.now(
                " ) {\n" + (indentStr * (indent + 1)) + bodyStr + "\n" + (indentStr * indent) + "}"
              )
            } else {
              Eval.now("for( ") |+| bindsString |+| Eval.now(" ) {" + bodyStr + "}")
            }
          }

      case b: Bundle =>
        Eval.now(BundleOps.showInstance.show(b) + "{ " + (indentStr * (indent + 1))) |+|
          buildStringM(b.body, indent + 1) |+| Eval.now(" }")

      case n: New =>
        val introducedNewsShiftIdx = (0 until n.bindCount).map(i => i + boundShift)
        Eval
          .now("new " + buildVariables(n.bindCount) + " in {\n" + indentStr * (indent + 1)) |+| this
          .copy(
            boundShift = boundShift + n.bindCount,
            newsShiftIndices = newsShiftIndices ++ introducedNewsShiftIdx
          )
          .buildStringM(n.p, indent + 1) |+|
          Eval.now("\n" + (indentStr * indent) + "}")

      case e: Expr =>
        buildStringM(e)

      case m: Match =>
        Eval.now("match ") |+| buildStringM(m.target) |+| Eval.now(" {\n") |+|
          m.cases.zipWithIndex.foldLeft(Eval.now("")) {
            case (string, (matchCase, i)) =>
              string |+| Eval.now(indentStr * (indent + 1)) |+| buildMatchCase(
                matchCase,
                indent + 1
              ) |+| Eval.now {
                if (i != m.cases.length - 1) "\n"
                else ""
              }
          } |+| Eval.now("\n" + (indentStr * indent) + "}")

      case u: GUnforgeable => buildStringM(u)
      case c: Connective =>
        c.connectiveInstance match {
          case ConnectiveInstance.Empty => Eval.now("")
          case ConnAndBody(value) =>
            Eval.now("{") |+| value.ps
              .map(buildStringM)
              .toList
              .intercalate(Eval.now(" /\\ ")) |+| Eval.now("}")
          case ConnOrBody(value) =>
            Eval.now("{") |+| value.ps
              .map(buildStringM)
              .toList
              .intercalate(Eval.now(" \\/ ")) |+| Eval.now("}")
          case ConnNotBody(value) => Eval.now("~{") |+| buildStringM(value) |+| Eval.now("}")
          case VarRefBody(value)  => Eval.now(s"=$freeId${freeShift - value.index - 1}")
          case _: ConnBool        => Eval.now("Bool")
          case _: ConnInt         => Eval.now("Int")
          case _: ConnBigInt      => Eval.now("BigInt")
          case _: ConnString      => Eval.now("String")
          case _: ConnUri         => Eval.now("Uri")
          case _: ConnByteArray   => Eval.now("ByteArray")
        }

      case par: Par =>
        if (isEmpty(par)) Eval.now("Nil")
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
          list.foldLeft((false, Eval.now(""))) {
            // format: off
            case ((prevNonEmpty, string), items) =>
              if (items.nonEmpty) {
                (true,
                 string |+| Eval.now { if (prevNonEmpty) " |\n" + (indentStr * indent) else "" } |+|
                    items.zipWithIndex.foldLeft(Eval.now("")) {
                     case (_string, (_par, index)) =>
                       _string |+| buildStringM(_par, indent) |+| Eval.now {
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

  private def buildSeq[T <: GeneratedMessage](s: Seq[T]): Eval[String] =
    s.zipWithIndex.foldLeft(Eval.now("")) {
      case (string, (p, i)) =>
        string |+| buildStringM(p) |+| Eval.now {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  private def buildPattern(patterns: Seq[Par]): Eval[String] =
    patterns.zipWithIndex.foldLeft(Eval.now("")) {

      case (string, (pattern, i)) =>
        string |+| buildChannelStringM(pattern) |+| Eval.now {
          if (i != patterns.length - 1) ", "
          else ""
        }
    }

  private def buildMatchCase(matchCase: MatchCase, indent: Int): Eval[String] = {
    val patternFree: Int = matchCase.freeCount
    val openBrace        = Eval.now(s"{\n${indentStr * (indent + 1)}")
    val closeBrace       = Eval.now(s"\n${indentStr * indent}}")
    this
      .copy(
        freeShift = boundShift,
        boundShift = 0,
        freeId = boundId,
        baseId = setBaseId()
      )
      .buildStringM(matchCase.pattern, indent) |+| Eval.now(" => ") |+| openBrace |+|
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
