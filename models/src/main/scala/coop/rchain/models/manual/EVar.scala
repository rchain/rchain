package coop.rchain.models.manual

/** A variable used as a var should be bound in a process context, not a name
  * context. For example:
  * `for (&#64;x &lt;- c1; &#64;y &lt;- c2) { z!(x + y) }` is fine, but
  * `for (x &lt;- c1; y &lt;- c2) { z!(x + y) }` should raise an error.
  */
final case class EVar(
    v: Var = Var.defaultInstance
)
