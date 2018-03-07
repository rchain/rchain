// Normalization:
// Normalization requires the evaluation of everything that doesn't go through
// the tuplespace, so the top level of a normalized process must have everything

package coop.rchain.rholang.interpreter

import scala.collection.immutable.BitSet
import scala.language.implicitConversions

case class Par(
    sends: List[Send],
    receives: List[Receive],
    // selects: List[Select],
    evals: List[Eval],
    news: List[New],
    exprs: List[Expr],
    matches: List[Match],
    id: List[GPrivate],
    freeCount: Int, // Makes pattern matching faster.
    locallyFree: BitSet
) {
  def this() =
    this(List(), List(), List(), List(), List(), List(), List(), 0, BitSet())
  // Single element convenience constructors
  def this(s: Send) =
    this(List(s), List(), List(), List(), List(), List(), List(), s.freeCount, s.locallyFree)
  def this(r: Receive) =
    this(List(), List(r), List(), List(), List(), List(), List(), r.freeCount, r.locallyFree)
  def this(e: Eval) =
    this(List(), List(), List(e), List(), List(), List(), List(), e.freeCount, e.locallyFree)
  def this(n: New) =
    this(List(), List(), List(), List(n), List(), List(), List(), n.freeCount, n.locallyFree)
  def this(e: Expr) =
    this(List(), List(), List(), List(), List(e), List(), List(), e.freeCount, e.locallyFree)
  def this(m: Match) =
    this(List(), List(), List(), List(), List(), List(m), List(), m.freeCount, m.locallyFree)
  def this(gPrivate: GPrivate) =
    this(List(), List(), List(), List(), List(), List(), List(gPrivate), 0, BitSet())

  // Convenience prepend methods
  def prepend(s: Send): Par =
    this.copy(sends = s :: this.sends,
              freeCount = this.freeCount + s.freeCount,
              locallyFree = this.locallyFree | s.locallyFree)
  def prepend(r: Receive): Par =
    this.copy(receives = r :: this.receives,
              freeCount = this.freeCount + r.freeCount,
              locallyFree = this.locallyFree | r.locallyFree)
  def prepend(e: Eval): Par =
    this.copy(evals = e :: this.evals,
              freeCount = this.freeCount + e.freeCount,
              locallyFree = this.locallyFree | e.locallyFree)
  def prepend(n: New): Par =
    this.copy(news = n :: this.news,
              freeCount = this.freeCount + n.freeCount,
              locallyFree = this.locallyFree | n.locallyFree)
  def prepend(e: Expr): Par =
    this.copy(exprs = e :: this.exprs,
              freeCount = this.freeCount + e.freeCount,
              locallyFree = this.locallyFree | e.locallyFree)
  def prepend(m: Match): Par =
    this.copy(matches = m :: this.matches,
              freeCount = this.freeCount + m.freeCount,
              locallyFree = this.locallyFree | m.locallyFree)
  def prepend(g: GPrivate): Par =
    this.copy(id = g :: this.id)

  def singleEval(): Option[Eval] =
    if (sends.isEmpty && receives.isEmpty && news.isEmpty && exprs.isEmpty && matches.isEmpty) {
      evals match {
        case List(single) => Some(single)
        case _            => None
      }
    } else {
      None
    }

  def singleNew(): Option[New] =
    if (sends.isEmpty && receives.isEmpty && evals.isEmpty && exprs.isEmpty && matches.isEmpty) {
      news match {
        case List(single) => Some(single)
        case _            => None
      }
    } else {
      None
    }

  def ++(that: Par) =
    Par(
      sends ++ that.sends,
      receives ++ that.receives,
      evals ++ that.evals,
      news ++ that.news,
      exprs ++ that.exprs,
      matches ++ that.matches,
      id ++ that.id,
      that.freeCount + freeCount,
      that.locallyFree | locallyFree
    )

  def isEmpty: Boolean =
    sends.isEmpty &&
      receives.isEmpty &&
      evals.isEmpty &&
      news.isEmpty &&
      matches.isEmpty &&
      exprs.isEmpty &&
      id.isEmpty

  def nonEmpty: Boolean = !isEmpty

}

object Par {
  def apply(): Par            = new Par()
  def apply(s: Send): Par     = new Par(s)
  def apply(r: Receive): Par  = new Par(r)
  def apply(e: Eval): Par     = new Par(e)
  def apply(n: New): Par      = new Par(n)
  def apply(e: Expr): Par     = new Par(e)
  def apply(m: Match): Par    = new Par(m)
  def apply(g: GPrivate): Par = new Par(g)

  implicit def fromSend(s: Send): Par                = apply(s)
  implicit def fromReceive(r: Receive): Par          = apply(r)
  implicit def fromEval(e: Eval): Par                = apply(e)
  implicit def fromNew(n: New): Par                  = apply(n)
  implicit def fromExpr(e: Expr): Par                = apply(e)
  implicit def fromMatch(m: Match): Par              = apply(m)
  implicit def fromGPrivate(gPrivate: GPrivate): Par = apply(gPrivate)
}

sealed trait Channel {
  def freeCount: Int
  def locallyFree: BitSet
}
case class Quote(p: Par) extends Channel {
  def freeCount: Int      = p.freeCount
  def locallyFree: BitSet = p.locallyFree
}
case class ChanVar(cvar: Var) extends Channel {
  def freeCount: Int      = cvar.freeCount
  def locallyFree: BitSet = cvar.locallyFree
}

// While we use vars in both positions, when producing the normalized
// representation we need a discipline to track whether a var is a name or a
// process.
// These are DeBruijn levels
sealed trait Var {
  def freeCount: Int
  def locallyFree: BitSet
}
case class BoundVar(level: Int) extends Var {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet(level)
}
// Wildcards are represented as bound variables. The initial normalization will
// not produce uses of the variable, but for (_ <- x) P is the same as
// for (y <- x) P if y is not free in P. We model that equivalence by turning all
// wildcards into bound variables.
// Variables that occur free in Par used as a pattern or ChanVar are binders.
// For the purpose of comparing patterns, we count just like BoundVars.

// In the DeBruijn level paper, they use negatives, but this is more clear.
case class FreeVar(level: Int) extends Var {
  def freeCount: Int      = 1
  def locallyFree: BitSet = BitSet()
}

// Upon send, all free variables in data are substituted with their values.
// also if a process is sent, it is auto-quoted.
case class Send(chan: Channel,
                data: List[Par],
                persistent: Boolean,
                freeCount: Int,
                locallyFree: BitSet)

// [Par] is an n-arity Pattern.
// It's an error for free Variable to occur more than once in a pattern.
// Don't currently support conditional receive
// Count is the number of free variables in the formals
case class Receive(binds: List[(List[Channel], Channel)],
                   body: Par,
                   persistent: Boolean,
                   bindCount: Int,
                   freeCount: Int,
                   locallyFree: BitSet)

case class Eval(channel: Channel) {
  def freeCount: Int      = channel.freeCount
  def locallyFree: BitSet = channel.locallyFree
}

// Number of variables bound in the new statement.
// For normalized form, p should not contain solely another new.
// Also for normalized form, the first use should be level+0, next use level+1
// up to level+count for the last used variable.
case class New(bindCount: Int, p: Par, locallyFree: BitSet) {
  def freeCount: Int = p.freeCount
}

case class Match(value: Par, cases: List[(Par, Par)], freeCount: Int, locallyFree: BitSet)

// Any process may be an operand to an expression.
// Only processes equivalent to a ground process of compatible type will reduce.
sealed trait Expr {
  def freeCount: Int
  def locallyFree: BitSet
}
sealed trait Ground                                                         extends Expr
case class EList(ps: List[Par], freeCount: Int, locallyFree: BitSet)        extends Ground
case class ETuple(ps: List[Par], freeCount: Int, locallyFree: BitSet)       extends Ground
case class ESet(ps: List[Par], freeCount: Int, locallyFree: BitSet)         extends Ground
case class EMap(kvs: List[(Par, Par)], freeCount: Int, locallyFree: BitSet) extends Ground
// A variable used as a var should be bound in a process context, not a name
// context. For example:
// for (@x <- c1; @y <- c2) { z!(x + y) } is fine, but
// for (x <- c1; y <- c2) { z!(x + y) } should raise an error.
case class EVar(v: Var) extends Expr {
  def freeCount: Int      = v.freeCount
  def locallyFree: BitSet = v.locallyFree
}
case class ENot(p: Par) extends Expr {
  def freeCount: Int      = p.freeCount
  def locallyFree: BitSet = p.locallyFree
}
case class ENeg(p: Par) extends Expr {
  def freeCount: Int      = p.freeCount
  def locallyFree: BitSet = p.locallyFree
}
case class EMult(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EDiv(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EPlus(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EMinus(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class ELt(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class ELte(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EGt(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EGte(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EEq(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class ENeq(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EAnd(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}
case class EOr(p1: Par, p2: Par) extends Expr {
  def freeCount: Int      = p1.freeCount + p2.freeCount
  def locallyFree: BitSet = p1.locallyFree | p2.locallyFree
}

case class GBool(b: Boolean) extends Ground {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet()
}
case class GInt(i: Integer) extends Ground {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet()
}
case class GString(s: String) extends Ground {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet()
}
case class GUri(u: String) extends Ground {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet()
}
// These should only occur as the program is being evaluated. There is no way in
// the grammar to construct them.
case class GPrivate(p: String) extends Ground {
  def freeCount: Int      = 0
  def locallyFree: BitSet = BitSet()
}

object GPrivate {
  def apply(): GPrivate = GPrivate(java.util.UUID.randomUUID.toString)
}
