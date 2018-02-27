package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.rblstring._
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, PC, RblBool, RblString, Tuple}
import org.scalatest._

class RblStringSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(1, Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  /** Case Sensistive compares */
  /** string= */
  "stringEq" should "return true if the strings match" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do not match" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringEq.fn(newCtxt) should be('left)
  }

  /** string!= */
  "stringNEq" should "return true if the strings do not match" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringNEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do match" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringNEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringNEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringNEq.fn(newCtxt) should be('left)
  }

  /** string< */
  "stringLess" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLess.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringLess.fn(newCtxt) should be('left)
  }

  /** string<= */
  "stringLEq" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringLEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringLEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringLEq.fn(newCtxt) should be('left)
  }

  /** string> */
  "stringGtr" should "return true if the left string is greater than the right one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGtr.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringGtr.fn(newCtxt) should be('left)
  }

  /** string>= */
  "stringGEq" should "return true if the left string is greater than the right one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringGEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringGEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringGEq.fn(newCtxt) should be('left)
  }

  /** Case Insensitive compares */
  /** string-ci= */
  "stringCiEq" should "return true if the strings match" in {
    val s1   = "abcdef"
    val s2   = "aBcDeF"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do not match" in {
    val s1   = "aBcDeF"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiEq.fn(newCtxt) should be('left)
  }

  /** string-ci!= */
  "stringCiNEq" should "return true if the strings do not match" in {
    val s1   = "AbCdEf"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiNEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do match" in {
    val s1   = "AbCdEf"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiNEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiNEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiNEq.fn(newCtxt) should be('left)
  }

  /** string-ci< */
  "stringCiLess" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "GhIjKl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLess.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "AbCdEf"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "GhIjKl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiLess.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiLess.fn(newCtxt) should be('left)
  }

  /** string-ci<= */
  "stringCiLEq" should "return true if the left string is less than the right one" in {
    val s1   = "AbCdEf"
    val s2   = "ghijkl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "aBcDeF"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "GhIjKl"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiLEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiLEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiLEq.fn(newCtxt) should be('left)
  }

  /** string-ci> */
  "stringCiGtr" should "return true if the left string is greater than the right one" in {
    val s1   = "gHiJkL"
    val s2   = "abcdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGtr.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "AbCdEf"
    val s2   = "aBcDeF"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "AbCdEf"
    val s2   = "gHiJkL"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiGtr.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiGtr.fn(newCtxt) should be('left)
  }

  /** string-ci>= */
  "stringCiGEq" should "return true if the left string is greater than the right one" in {
    val s1   = "GHIjkl"
    val s2   = "abcDEF"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcDEF"
    val s2   = "ABCdef"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGEq.fn(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "aBCDef"
    val s2   = "ghIJKl"
    val strs = Seq(RblString(s1), RblString(s2))

    val newCtxt =
      ctxt.copy(
        nargs = 2,
        argvec = Tuple(strs)
      )
    stringCiGEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(RblString("foo")), Fixnum(42)))
    stringCiGEq.fn(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    stringCiGEq.fn(newCtxt) should be('left)
  }

}
