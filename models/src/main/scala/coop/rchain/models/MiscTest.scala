package coop.rchain.models

import org.scalatest.FlatSpec

import scala.concurrent.SyncVar

class MiscTest extends FlatSpec {

  it should "work" in {

    case class State(value: Int)

    class Checkpoint(state: SyncVar[State]) {
      def update(newValue: Int): 
    }

    val state: SyncVar[State] = new SyncVar[State]()

    state.put(State(10))

  }

}
