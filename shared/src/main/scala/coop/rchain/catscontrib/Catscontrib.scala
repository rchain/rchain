package coop.rchain.catscontrib

trait Opses
    extends ToMonadOps
    with ToBooleanOps
    with ToOptionOps
    with ToApplicativeError_Ops
    with ToTaskableOps
    with ToFuturableOps {}

object Catscontrib extends Opses
