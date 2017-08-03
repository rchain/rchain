/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable.ArrayBuffer

object TermType extends Enumeration {
  val Key = Value("Key")
  val Params = Value("Params")
  val Variable = Value("Variable")
  val Numeral = Value("Numeral")
  val Error = Value("Error")
}

trait TermTree {
  var term: String // entire term, including params of Keys
  val termType: TermType.Value

  def isKey: Boolean = { termType == TermType.Key }
  def isParams: Boolean = { termType == TermType.Params }
  def isVariable: Boolean = { termType == TermType.Variable }
  def isNumeral: Boolean = { termType == TermType.Numeral }

  def display: Unit
}

class Params(paramsArray: ArrayBuffer[TermTree]) extends TermTree {
  val params = paramsArray
  var term = "("
  for (i <- 0 until params.length - 1) {
    term += params(i).term + ","
  }
  term += params(params.length - 1).term + ")"

  val length = paramsArray.length

  val termType = TermType.Params

  def display: Unit = { print(term) }
}

// Atoms are variables and numerals but not keys

trait ParamAtom extends TermTree {}

class Variable(paramIn: String) extends ParamAtom {
  val variable = paramIn.trim
  if (!variable.forall(Character.isLetter)) {
    throw new Exception(s"Variable(): malformed param: '$paramIn'")
  }

  var term = variable
  val termType = TermType.Variable

  def display: Unit = { print("'" + variable + "'") }
}

class Numeral(paramIn: String) extends ParamAtom {
  val numeral = paramIn.trim
  if (!numeral.forall(Character.isDigit)) {
    throw new Exception(s"Numeral(): malformed param: '$paramIn'")
  }

  var term = numeral
  val termType = TermType.Numeral

  def display: Unit = { print("'" + numeral + "'") }
}

object TermTools {
  def makeParamAtom(atom: String): ParamAtom = {
    if (isVariable(atom))
      return new Variable(atom)
    else if (isNumeral(atom))
      return new Numeral(atom)

    throw new Exception(s"TermTree.makeParamAtom(): unexpected input: '$atom'")
  }

  def isVariable(s: String): Boolean = {
    s.forall(Character.isLetter)
  }
  def isNumeral(s: String): Boolean = {
    s.forall(Character.isDigit)
  }

}
