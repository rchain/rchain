package coop.rchain.rosette.utils.opcodes

import coop.rchain.rosette.{Op, VMState, VirtualMachine}
import org.scalatest.WordSpec
import shapeless.{::, ops, HList, HNil, Witness}

object Dsl {
  def defaultFormatter(name: String, path: String): String =
    s"modify state.$path on $name"

  private def witnessName: Witness => String = _.value.toString.substring(1)

  private def buildPath(l: List[Witness]): String =
    l.map(witnessName).mkString(".")

  class theState[Path <: HList] private {

    def on(op: Op,
           state: VMState = testState,
           formatter: (String, String) => String = defaultFormatter)(
        f: Any => Unit)(implicit sel: DeepSelector[Path, VMState],
                        m: WordSpec): Unit = {

      val newState = dispatch(op, state)
      sel(newState) match {
        case (l: List[Witness], r) =>
          val path = buildPath(l)
          val name = op.getClass.getSimpleName
          val message = formatter(name, path)
          m.registerTest(message)(f(r))
      }
    }

    def >>[S <: scala.Symbol](acc: Witness.Lt[S])(
        implicit append: ops.hlist.Prepend[Path, S :: HNil]) =
      new theState[append.Out]
  }

  object theState extends theState[HNil]

  def dispatch(op: Op, state: VMState = testState): VMState =
    VirtualMachine.executeDispatch(op, state)
}
