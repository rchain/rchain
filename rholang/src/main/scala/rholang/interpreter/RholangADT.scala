// Normalization:
// Normalization requires the evaluation of everything that doesn't go through
// the tuplespace, so the top level of a normalized process must have everything

package coop.rchain.interpreter

case class Par(
  sends: List[Send],
  receives: List[Receive],
  // selects: List[Select],
  evals: List[Eval],
  news: List[New],
  expr: List[Expr]
  // matches: List[Match]
) {
  // TODO: write helper methods to append an X and return a new par
  // TODO: write helper method to get an empty par
  def this() =
    this(List(), List(), List(), List(), List())
}

object Par {
  def apply() : Par = new Par()
}

sealed trait Channel
case class Quote(p: Par) extends Channel
case class ChanVar(cvar: Var) extends Channel

// While we use vars in both positions, when producing the normalized
// representation we need a discipline to track whether a var is a name or a
// process.
// These are DeBruijn levels
sealed trait Var
case class BoundVar(level: Int) extends Var
// It is an error to use WildCard() in a non-binding position
case class WildCard() extends Var
// Variables that occur free in Par used as a pattern or ChanVar are binders.
// For the purpose of comparing patterns, we count just like BoundVars.

// In the DeBruijn level paper, they use negatives, but this is more clear.
case class FreeVar(level: Int) extends Var

// Upon send, all free variables in data are substituted with their values.
// also if a process is sent, it is auto-quoted.
case class Send(chan: Channel, data: Channel)

// [Par] is an n-arity Pattern.
// It's an error for free Variable to occur more than once in a pattern.
// Don't currently support conditional receive
case class Receive(binds: List[(List[Par], Channel)])

case class Eval(channel: Channel)

// Number of variables bound in the new statement.
// For normalized form, p should not contain solely another new.
// Also for normalized form, the first use should be level+0, next use level+1
// up to level+count for the last used variable.
case class New(count: Int, p: Par)

// Any process may be an operand to an expression.
// Only processes equivalent to a ground process of compatible type will reduce.
sealed trait Expr
sealed trait Ground extends Expr
case class EList(ps: List[Par]) extends Ground
case class ETuple(ps: List[Par]) extends Ground
case class ESet(ps: List[Par]) extends Ground
case class EMap(kvs: List[(Par,Par)]) extends Ground
case class ENeg(p: Par) extends Expr
// A variable used as a var should be bound in a process context, not a name
// context. For example:
// for (@x <- c1; @y <- c2) { z!(x + y) } is fine, but
// for (x <- c1; y <- c2) { z!(x + y) } should raise an error.
case class EVar(v: Var) extends Expr
case class EMult(p1: Par, p2: Par) extends Expr
case class EDiv(p1: Par, p2: Par) extends Expr
case class EPlus(p1: Par, p2: Par) extends Expr
case class EMinus(p1: Par, p2: Par) extends Expr
case class ELt(p1: Par, p2: Par) extends Expr
case class ELte(p1: Par, p2: Par) extends Expr
case class EGt(p1: Par, p2: Par) extends Expr
case class EGte(p1: Par, p2: Par) extends Expr
case class EEq(p1: Par, p2: Par) extends Expr
case class ENeq(p1: Par, p2: Par) extends Expr

case class GBool(b: Boolean) extends Ground
case class GInt(i: Integer) extends Ground
case class GString(s: String) extends Ground
case class GUri(u: String) extends Ground
// These should only occur as the program is being evaluated. There is no way in
// the grammar to construct them.
case class GPrivate(p: String) extends Ground
