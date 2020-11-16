package coop.rchain.rholang.interpreter.compiler

final case class SourcePosition(row: Int, column: Int) {
  override def toString: String = s"$row:$column"
}
