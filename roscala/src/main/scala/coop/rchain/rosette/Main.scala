package coop.rchain.rosette

import java.io.InputStreamReader

import coop.rchain.rosette.parser.bytecode._
import Show._

import scala.collection.mutable

object Main extends App {
  val bytes = mutable.ArrayBuffer[Int]()
  val stdin = new InputStreamReader(System.in)

  Stream.continually(stdin.read()).takeWhile(_ != -1).foreach(bytes += _)

  Parser.parse(bytes) match {
    case Right(opCodes) =>
      val initState = VMState(Map(),
                              Code(null, null),
                              Ctxt.PLACEHOLDER,
                              Location.PLACEHOLDER,
                              PC(0))

      val exitState = VirtualMachine.executeSeq(opCodes, initState)

      System.exit(exitState.exitCode)

    case Left(error) => println(error.show)
  }
}
