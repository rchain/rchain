package coop.rchain.rosette.parser.bytecode

import cats.implicits._
import coop.rchain.rosette._

sealed trait ParseError
case class UnknownOpCode(byte: Int) extends ParseError
case object MissingArgument extends ParseError

object Parser {
  def parse(bytes: Seq[Int]): Either[ParseError, Seq[Op]] = {
    lineUp(bytes) match {
      case Some(list) =>
        val ops = list.map(seq =>
          seq.head match {
            case byte if byte.hasByteFormat("00000000") =>
              Right(OpHalt())

            case byte if byte.hasByteFormat("00000001") =>
              Right(OpPush())

            case byte if byte.hasByteFormat("00000010") =>
              Right(OpPop())

            case byte if byte.hasByteFormat("00000011") =>
              Right(OpNargs(seq(1)))

            case byte if byte.hasByteFormat("00000100") =>
              Right(OpAlloc(seq(1)))

            case byte if byte.hasByteFormat("00000101") =>
              Right(OpPushAlloc(seq(1)))

            case byte if byte.hasByteFormat("00000110") =>
              Right(OpExtend(seq(1)))

            case byte if byte.matches("000010pp") =>
              val pHighTwo = byte.slice(0, 2)
              val pLowEight = seq(1)

              val p = pHighTwo.concat(pLowEight)
              val n = seq(2)

              Right(OpOutstanding(p, n))

            case byte if byte.matches("000011pp") =>
              val p = seq(1)

              Right(OpFork(p))

            case byte if byte.matches("000100un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1).highNibble
              val v = seq(1).lowNibble

              Right(OpXmitTag(u, n, m, v))

            case byte if byte.matches("000101un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1).highNibble
              val a = seq(1).lowNibble

              Right(OpXmitArg(u, n, m, a))

            case byte if byte.matches("000110un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1).highNibble
              val r = seq(1).lowNibble

              Right(OpXmitReg(u, n, m, r))

            case byte if byte.matches("000111un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)

              Right(OpXmit(u, n, m))

            case byte if byte.matches("001000un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val v = seq(2)

              Right(OpXmitTagXtnd(u, n, m, v))

            case byte if byte.matches("001001un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val a = seq(2)

              Right(OpXmitArgXtnd(u, n, m, a))

            case byte if byte.matches("001010un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val r = seq(2)

              Right(OpXmitRegXtnd(u, n, m, r))

            case byte if byte.matches("001011un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)

              Right(OpSend(u, n, m))

            case byte if byte.matches("001100un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val k = seq(2)
              val v = seq(3)

              Right(OpApplyPrimTag(u, n, m, k, v))

            case byte if byte.matches("001101un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val k = seq(2)
              val a = seq(3)

              Right(OpApplyPrimArg(u, n, m, k, a))

            case byte if byte.matches("001110un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val k = seq(2)
              val r = seq(3)

              Right(OpApplyPrimReg(u, n, m, k, r))

            case byte if byte.matches("001111un") =>
              val n = byte.bit(0)
              val u = byte.bit(1)
              val m = seq(1)
              val k = seq(2)

              Right(OpApplyCmd(u, n, m, k))

            case byte if byte.matches("010000xn") =>
              val n = byte.bit(0)
              val v = seq(1)

              Right(OpRtnTag(n, v))

            case byte if byte.matches("010001xn") =>
              val n = byte.bit(0)
              val a = seq(1)

              Right(OpRtnArg(n, a))

            case byte if byte.matches("010010xn") =>
              val n = byte.bit(0)
              val r = seq(1)

              Right(OpRtnReg(n, r))

            case byte if byte.matches("010011xn") =>
              val n = byte.bit(0)

              Right(OpRtn(n))

            case byte if byte.matches("010100xn") =>
              val n = byte.bit(0)
              val v = seq(1)

              Right(OpUpcallRtn(n, v))

            case byte if byte.hasByteFormat("01010100") =>
              Right(OpUpcallResume())

            case byte if byte.matches("011000nn") =>
              val nHighTwo = byte.slice(0, 2)
              val nLowEight = seq(1)

              val n = nHighTwo.concat(nLowEight)

              Right(OpJmp(n))

            case byte if byte.matches("011001nn") =>
              val nHighTwo = byte.slice(0, 2)
              val nLowEight = seq(1)

              val n = nHighTwo.concat(nLowEight)

              Right(OpJmpFalse(n))

            case byte if byte.matches("011010nn") =>
              val nHighTwo = byte.slice(0, 2)
              val nLowEight = seq(1)

              val n = nHighTwo.concat(nLowEight)
              val m = seq(2)

              Right(OpJmpCut(n, m))

            case byte if byte.matches("0111aaaa") =>
              val a = byte.lowNibble
              val v = seq(1)

              Right(OpLookupToArg(a, v))

            case byte if byte.matches("1000rrrr") =>
              val r = byte.lowNibble
              val v = seq(1)

              Right(OpLookupToReg(r, v))

            case byte if byte.matches("1001illl") =>
              val i = byte.bit(3)
              val l = byte.slice(0, 3)
              val o = seq(1).highNibble
              val a = seq(1).lowNibble

              Right(OpXferLexToArg(i, l, o, a))

            case byte if byte.matches("1010illl") =>
              val i = byte.bit(3)
              val l = byte.slice(0, 3)
              val o = seq(1).highNibble
              val r = seq(1).lowNibble

              Right(OpXferLexToReg(i, l, o, r))

            case byte if byte.hasByteFormat("10110000") =>
              val a = seq(1)
              val g = seq(2).concat(seq(3))

              Right(OpXferGlobalToArg(a, g))

            case byte if byte.hasByteFormat("10110001") =>
              val r = seq(1).lowNibble
              val g = seq(2).concat(seq(3))

              Right(OpXferGlobalToReg(r, g))

            case byte if byte.hasByteFormat("10110010") =>
              val d = seq(1).highNibble
              val s = seq(1).lowNibble

              Right(OpXferArgToArg(d, s))

            case byte if byte.hasByteFormat("10110100") =>
              val a = seq(1)

              Right(OpXferRsltToArg(a))

            case byte if byte.hasByteFormat("10110101") =>
              val a = seq(1)

              Right(OpXferArgToRslt(a))

            case byte if byte.hasByteFormat("10110110") =>
              val r = seq(1)

              Right(OpXferRsltToReg(r))

            case byte if byte.hasByteFormat("10110111") =>
              val r = seq(1).lowNibble

              Right(OpXferRegToRslt(r))

            case byte if byte.hasByteFormat("10111000") =>
              val v = seq(1)

              Right(OpXferRsltToDest(v))

            case byte if byte.hasByteFormat("10111001") =>
              val v = seq(1)

              Right(OpXferSrcToRslt(v))

            case byte if byte.hasByteFormat("10111010") =>
              val v = seq(1).lowNibble
              val a = seq(1).highNibble

              Right(OpIndLitToArg(v, a))

            case byte if byte.hasByteFormat("10111011") =>
              val v = seq(1).lowNibble
              val r = seq(1).highNibble

              Right(OpIndLitToReg(r, v))

            case byte if byte.hasByteFormat("10111100") =>
              val v = seq(1)

              Right(OpIndLitToRslt(v))

            case byte if byte.matches("1100vvvv") =>
              val v = byte.slice(0, 4)
              val a = seq(1)

              Right(OpImmediateLitToArg(v, a))

            case byte if byte.matches("1101vvvv") =>
              val v = byte.slice(0, 4)
              val r = seq(1).lowNibble

              Right(OpImmediateLitToReg(v, r))

            case byte => Left(UnknownOpCode(byte))
        })

        ops.sequenceU

      case None => Left(MissingArgument)
    }
  }

  /** Line up opcodes with arguments */
  private def lineUp(bytes: Seq[Int]): Option[List[Seq[Int]]] =
    try {
      val linedUp = bytes
        .zip(Stream from 0)
        .foldLeft[Seq[Seq[Int]]](Seq()) {
          case (seq, (byte, i)) =>
            if (!isArg(i, seq)) {
              val opcodeWithArgs = byte match {
                // These opcodes have three-byte arguments
                case op
                    if op.matches("000010pp")
                      || op.matches("00100xun")
                      || op.matches("001010un")
                      || op.matches("0011xxun")
                      || op.matches("011010nn")
                      || op.matches("1011000x") =>
                  Seq(op, bytes(i + 1), bytes(i + 2), bytes(i + 3), i + 3)

                // All other opcodes have a one-byte argument
                case op => Seq(op, bytes(i + 1), i + 1)
              }

              seq :+ opcodeWithArgs
            } else {
              seq
            }
        }

      // Remove helper indices from inner seqs
      Some(linedUp.map(_.dropRight(1)).toList)
    } catch {
      case _: IndexOutOfBoundsException => None
    }

  private def isArg(index: Int, seq: Seq[Seq[Int]]): Boolean =
    try {
      val indexOfLastArg = seq.last.last

      if (index > indexOfLastArg) false else true
    } catch {
      case _: NoSuchElementException => false
    }

}
