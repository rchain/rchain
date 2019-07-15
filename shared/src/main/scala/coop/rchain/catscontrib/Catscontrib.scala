package coop.rchain.catscontrib

trait Opses
    extends ToMonadOps
    with ToBooleanOps
    with ToBooleanF
    with ToOptionOps
    with ToApplicativeError_Ops
    with ToTaskableOps

object Catscontrib extends Opses
