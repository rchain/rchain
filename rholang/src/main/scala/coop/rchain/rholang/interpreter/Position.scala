package coop.rchain.rholang.interpreter

case class Position(row: Int, column: Int, offset: Int)
object Position {
  def empty: Position = Position(0, 0, 0)

}
