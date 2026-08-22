package coop.rchain.roscala.util

import coop.rchain.roscala._
import coop.rchain.roscala.ob.{Ob, Symbol => RSymbol}

object misc {
  type SymbolOffsets = Map[Symbol, Int]

  def numberSuffix(n: Int): String = n match {
    case 1 => "st"
    case 2 => "nd"
    case 3 => "rd"
    case _ => "th"
  }

  def properPrep(s: String): String = {
    def isVowel = Set('a', 'e', 'i', 'o', 'u')
    s.charAt(0).toLower match {
      case c if isVowel(c) => "an"
      case _               => "a"
    }
  }

  def createGlobalEnv(entries: Map[RSymbol, Ob]): (GlobalEnv, SymbolOffsets) = {
    val globalEnv = new GlobalEnv()

    // contains the position of a `Symbol` in the `globalEnv`
    val symbolPosition = entries.map {
      case (key, value) =>
        val i = globalEnv.addSlot(key, value)
        (Symbol(key.value), i)
    }

    (globalEnv, symbolPosition)
  }

  implicit class SymbolOps(sym: Symbol) {
    def global(implicit symbolOffsets: SymbolOffsets): Int = symbolOffsets(sym)
  }

  class OpcodePrettyPrinter {
    var debugIndent = 0

    def print(pc: Int, opcode: Opcode): String = {
      val s = ("  " * debugIndent) + pc + ": " + opcode.toString
      debugIndent = indent(debugIndent, opcode)
      s
    }

    private def indent(value: Int, opcode: Opcode): Int = {
      def ifNext(next: Boolean) = if (next) 0 else value

      opcode match {
        case OpPush | OpPushAlloc(_)  => value + 1
        case OpPop                    => value - 1
        case OpRtn(next)              => ifNext(next)
        case OpRtnArg(next, _)        => ifNext(next)
        case OpRtnReg(next, _)        => ifNext(next)
        case OpRtnTag(next, _)        => ifNext(next)
        case OpXmit(_, next, _)       => ifNext(next)
        case OpXmitArg(_, next, _, _) => ifNext(next)
        case OpXmitReg(_, next, _, _) => ifNext(next)
        case OpXmitTag(_, next, _, _) => ifNext(next)
        case _                        => value
      }
    }
  }
}
