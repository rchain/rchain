package coop.rchain.models.rholangN

final class ENegN(val p: ParN) extends OperationN
object ENegN { def apply(p: ParN): ENegN = new ENegN(p) }
