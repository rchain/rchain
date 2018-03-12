package coop.rchain.models

import com.trueaccord.scalapb.{GeneratedMessage, TextFormat}
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}

object PrettyPrinter {
  def prettyPrint(e: Expr): Unit =
    e.exprInstance match {
      case ENegBody(ENeg(p)) =>
        print("-")
        prettyPrint(p.get)
      case EMultBody(EMult(p1, p2)) =>
        prettyPrint(p1.get)
        print(" * ")
        prettyPrint(p2.get)
      case EDivBody(EDiv(p1, p2)) =>
        prettyPrint(p1.get)
        print(" / ")
        prettyPrint(p2.get)
      case EPlusBody(EPlus(p1, p2)) =>
        prettyPrint(p1.get)
        print(" + ")
        prettyPrint(p2.get)
      case EMinusBody(EMinus(p1, p2)) =>
        prettyPrint(p1.get)
        print(" - ")
        prettyPrint(p2.get)

      case ENotBody(ENot(p)) =>
        print("~")
        prettyPrint(p.get)
      case EAndBody(EAnd(p1, p2)) =>
        prettyPrint(p1.get)
        print(" && ")
        prettyPrint(p2.get)
      case EOrBody(EOr(p1, p2)) =>
        prettyPrint(p1.get)
        print(" || ")
        prettyPrint(p2.get)

      case EEqBody(EEq(p1, p2)) =>
        prettyPrint(p1.get)
        print(" == ")
        prettyPrint(p2.get)
      case ENeqBody(ENeq(p1, p2)) =>
        prettyPrint(p1.get)
        print(" != ")
        prettyPrint(p2.get)
      case EGtBody(EGt(p1, p2)) =>
        prettyPrint(p1.get)
        print(" > ")
        prettyPrint(p2.get)
      case EGteBody(EGte(p1, p2)) =>
        prettyPrint(p1.get)
        print(" >= ")
        prettyPrint(p2.get)
      case ELtBody(ELt(p1, p2)) =>
        prettyPrint(p1.get)
        print(" < ")
        prettyPrint(p2.get)
      case ELteBody(ELte(p1, p2)) =>
        prettyPrint(p1.get)
        print(" <= ")
        prettyPrint(p2.get)

      case EListBody(EList(s, _, _)) =>
        print("[")
        printSeq(s)
        print("]")
      case ETupleBody(ETuple(s, _, _)) =>
        print("[")
        printSeq(s)
        print("]")
      case ESetBody(ESet(s, _, _)) =>
        print("(")
        printSeq(s)
        print(")")
      case EMapBody(EMap(kvs, _, _)) =>
        print("{")
        for { (kv, i) <- kvs.zipWithIndex } yield {
          prettyPrint(kv.key.get)
          print(":")
          prettyPrint(kv.value.get)
          if (i != kvs.length - 1)
            print(",")
        }
        print("}")

      case EVarBody(EVar(v)) =>
        prettyPrint(v.get)

      case GBool(b)   => print(b)
      case GInt(i)    => print(i)
      case GString(s) => print(s)
      case GUri(u)    => print(u)

      // TODO: Figure out if we can prevent ScalaPB from generating
      case ExprInstance.Empty => print("Nil")

      case _ => throw new Error("Attempted to print unknown Expr type")
    }

  def prettyPrint(v: Var): Unit =
    v.varInstance match {
      case FreeVar(level) =>
        print(s"z${level}")
      case BoundVar(level) =>
        print(s"x${level}")
      case Wildcard(_) =>
        print("_")
      // TODO: Figure out if we can prevent ScalaPB from generating
      case VarInstance.Empty =>
        print("@Nil")
    }

  def prettyPrint(c: Channel): Unit =
    c.channelInstance match {
      case Quote(p) =>
        print("@{ ")
        prettyPrint(p)
        print(" }")
      case ChanVar(cv) =>
        prettyPrint(cv)
      // TODO: Figure out if we can prevent ScalaPB from generating
      case ChannelInstance.Empty =>
        print("@Nil")
    }

  def prettyPrint(t: GeneratedMessage): Unit =
    t match {
      case v: Var     => prettyPrint(v)
      case c: Channel => prettyPrint(c)
      case s: Send =>
        prettyPrint(s.chan.get)
        print("!(")
        for { (datum, i) <- s.data.zipWithIndex } yield {
          prettyPrint(datum)
          if (i != s.data.length - 1)
            print(",")
        }
        print(")")
      case r: Receive =>
        print("for (")
        for { (bind, i) <- r.binds.zipWithIndex } yield {
          printSeq(bind.patterns)
          print(" <- ")
          prettyPrint(bind.source.get)
          if (i != r.binds.length - 1) {
            print("; ")
          }
        }
        print(") { ")
        prettyPrint(r.body.get)
        print(" }")
      case e: Eval =>
        print("*")
        prettyPrint(e.channel.get)
      case n: New =>
        print("new ")
        printNewVariables(n.bindCount)
        print(" in {")
        prettyPrint(n.p.get)
        print("}")
      case e: Expr =>
        prettyPrint(e)
      case m: Match =>
        print("match { ")
        prettyPrint(m.target.get)
        print(" } { ")
        for { (matchCase, i) <- m.cases.zipWithIndex } yield {
          prettyPrint(matchCase.pattern.get)
          print(" => ")
          prettyPrint(matchCase.source.get)
          if (i != m.cases.length - 1) {
            print("; ")
          }
        }
        print(" }")
      case g: GPrivate =>
        print(g.id)
      case p: Par =>
        if (isEmpty(p)) {
          print("Nil")
        } else {
          val list = List(p.sends, p.receives, p.evals, p.news, p.exprs, p.matches, p.ids)
          list.foldLeft(false)((acc: Boolean, items: Seq[GeneratedMessage]) => {
            if (items.nonEmpty) {
              if (acc)
                print(" | ")
              for { (item, i) <- items.zipWithIndex } yield {
                prettyPrint(item)
                if (i != items.length - 1) {
                  print(" | ")
                }
              }
              true
            } else {
              acc
            }
          })
        }
      case _ => throw new Error("Attempt to print unknown GeneratedMessage type")
    }

  // TODO: Calculate proper numbering/naming
  private def printNewVariables(bindCount: Int) = {
    // We arbitrarily limit the new variable printing count to MAX_NEW_VAR_COUNT
    // to prevent exploding the state of the shapeless generator
    val MAX_NEW_VAR_COUNT = 128
    printSeq(Range(0, List(MAX_NEW_VAR_COUNT, bindCount).min).map(x =>
      Expr(exprInstance = GString(s"_${x}"))))
  }

  private def printSeq[T <: GeneratedMessage](s: Seq[T]) =
    for { (p, i) <- s.zipWithIndex } yield {
      prettyPrint(p)
      if (i != s.length - 1)
        print(",")
    }

  private def isEmpty(p: Par) =
    p.sends.isEmpty && p.receives.isEmpty && p.evals.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.ids.isEmpty
}
