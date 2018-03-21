package coop.rchain.rosette

case class Template(meta: Ob, parent: Ob, keyTuple: Tuple, keyMeta: Ob, pat: CompoundPattern)
    extends Ob {
  def `match`(argvec: Tuple, nargs: Int): Option[Tuple] =
    pat.`match`(argvec, nargs)
}
