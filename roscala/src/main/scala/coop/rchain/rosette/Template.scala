package coop.rchain.rosette

case class Template(keyTuple: Tuple, keyMeta: Ob, pat: CompoundPattern)
    extends Ob {
  def `match`(argvec: Tuple, nargs: Int): Option[Tuple] =
    pat.`match`(argvec, nargs)
}
