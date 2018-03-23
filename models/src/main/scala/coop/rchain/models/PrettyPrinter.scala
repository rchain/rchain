package coop.rchain.models

import com.trueaccord.scalapb.GeneratedMessage
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}

object PrettyPrinter {
  def apply(): PrettyPrinter = PrettyPrinter(0, 0)
}

case class PrettyPrinter(freeShift: Int, boundShift: Int) {
  def buildString(e: Expr): String =
    e.exprInstance match {
      case ENegBody(ENeg(p)) => "-" + buildString(p.get)
      case EMultBody(EMult(p1, p2)) =>
        buildString(p1.get) + " * " + buildString(p2.get)
      case EDivBody(EDiv(p1, p2)) =>
        buildString(p1.get) + " / " + buildString(p2.get)
      case EPlusBody(EPlus(p1, p2)) =>
        buildString(p1.get) + " + " + buildString(p2.get)
      case EMinusBody(EMinus(p1, p2)) =>
        buildString(p1.get) + " - " + buildString(p2.get)
      case ENotBody(ENot(p)) => "~" + buildString(p.get)
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

      case EListBody(EList(s, _, _, _)) => "[" + buildSeq(s) + "]"

      case ETupleBody(ETuple(s, _, _, _)) => "[" + buildSeq(s) + "]"

      case ESetBody(ESet(s, _, _, _)) => "(" + buildSeq(s) + ")"

      case EMapBody(EMap(kvs, _, _, _)) =>
        "{" + (for { (kv, i) <- kvs.zipWithIndex } yield {
          val tempString = buildString(kv.key.get) + ":" + buildString(kv.value.get)
          if (i != kvs.length - 1) tempString + ","
          else tempString
        }) + "}"

      case EVarBody(EVar(v)) => buildString(v.get)

      case GBool(b)   => b.toString
      case GInt(i)    => i.toString
      case GString(s) => "\"" + s + "\""
      case GUri(u)    => s"`$u`"

      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => "Nil"

      case _ => throw new Error("Attempted to print unknown Expr type")
    }

  def buildString(v: Var): String =
    v.varInstance match {
      case FreeVar(level)  => s"x${level + freeShift}"
      case BoundVar(level) => s"x${boundShift - level - 1}"
      case Wildcard(_)     => "_"
      // TODO: Figure out if we can prevent ScalaPB from generating
      case VarInstance.Empty => "@Nil"
    }

  def buildString(c: Channel): String =
    c.channelInstance match {
      case Quote(p) =>
        "@{ " + buildString(p) + " }"
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
        } + {
          ("" /: s.data.zipWithIndex) {
            case (string, (datum, i)) =>
              string + buildString(datum) + {
                if (i != s.data.length - 1) ", "
                else ""
              }
          }
        } + ")"

      case r: Receive =>
        "for( " + {
          ("" /: r.binds.zipWithIndex) {
            case (string, (bind, i)) =>
              // Stepping past a channel increments boundShift by 1.
              // Entering a pattern sets freeShift = boundShift and boundShift = 0.
              string + PrettyPrinter(freeShift, boundShift + i).buildPattern(bind.patterns) + {
                if (r.persistent) " <= " + buildString(bind.source.get)
                else " <- " + buildString(bind.source.get)
              } + {
                if (i != r.binds.length - 1) " ; "
                else ""
              }
          }
        } + " ) { " + PrettyPrinter(freeShift, boundShift + r.binds.length)
          .buildString(r.body.get) + " }"

      case e: Eval => "*" + buildString(e.channel.get)
      case n: New =>
        "new " + buildNewVariables(n.bindCount) + " in { " + PrettyPrinter(
          freeShift,
          boundShift + n.bindCount).buildString(n.p.get) + " }"
      case e: Expr =>
        buildString(e)
      case m: Match =>
        "match { " + buildString(m.target.get) + " } { " + {
          for { (matchCase, i) <- m.cases.zipWithIndex } yield {
            buildString(matchCase.pattern.get) + " => " + buildString(matchCase.source.get) + {
              if (i != m.cases.length - 1) " ; "
              else ""
            }
          }
        } + " }"
      case g: GPrivate => g.id
      case p: Par =>
        if (isEmpty(p)) "Nil"
        else {
          val list = List(p.sends, p.receives, p.evals, p.news, p.exprs, p.matches, p.ids)
          ((false, "") /: list) {
            case ((prevNonEmpty, string), items) =>
              if (items.nonEmpty) {
                (true, string + { if (prevNonEmpty) " | " else "" } + ("" /: items.zipWithIndex) {
                  case (_string, (_p, i)) =>
                    _string + buildString(_p) + {
                      if (i != items.length - 1) " | " else ""
                    }
                })
              } else (prevNonEmpty, string)
          }
        }._2
      case _ => throw new Error("Attempt to print unknown GeneratedMessage type")
    }

  private def buildNewVariables(bindCount: Int): String = {
    // We arbitrarily limit the new variable string count to MAX_NEW_VAR_COUNT
    // to prevent exploding the state of the shapeless generator
    val MAX_NEW_VAR_COUNT = 128
    buildSeq(
      (0 until Math.min(MAX_NEW_VAR_COUNT, bindCount)).map(i => GPrivate(s"x${boundShift + i}")))
  }

  private def buildSeq[T <: GeneratedMessage](s: Seq[T]): String =
    ("" /: s.zipWithIndex) {
      case (string, (p, i)) =>
        string + buildString(p) + {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  def buildPattern(s: Seq[Channel]): String =
    ("" /: s.zipWithIndex) {
      case (string, (p, i)) =>
        string + PrettyPrinter(boundShift + i, 0).buildString(p) + {
          if (i != s.length - 1) ", "
          else ""
        }
    }

  private def isEmpty(p: Par) =
    p.sends.isEmpty & p.receives.isEmpty & p.evals.isEmpty & p.news.isEmpty & p.exprs.isEmpty & p.matches.isEmpty & p.ids.isEmpty
}
