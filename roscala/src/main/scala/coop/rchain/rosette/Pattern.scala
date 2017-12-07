package coop.rchain.rosette

class Pattern

class CompoundPattern extends Pattern {
  def matchPattern(tuple: Tuple, n: Int): Option[Tuple] =
    Some(Tuple.Placeholder)
}

case class Template(keytuple: Tuple, keymeta: Ob, pat: CompoundPattern)
    extends CompoundPattern
