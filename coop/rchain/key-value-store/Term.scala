/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable.ArrayBuffer

trait TermTree {
  var term: String // entire term, including params for Keys

  def isKey: Boolean = { this.isInstanceOf[Key] }
  def isVariable: Boolean = { this.isInstanceOf[Variable] }
  def isNumeral: Boolean = { this.isInstanceOf[Numeral] }

  def typeIs: String = {
    if (this.isKey) return ("Key")
    if (this.isVariable) return ("Variable")
    if (this.isNumeral) return ("Numeral")
    "Unknown"
  }

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

  def display: Unit = { print(term) }
}

trait Atom extends TermTree {}

class Variable(paramIn: String) extends Atom {
  val variable = paramIn.trim
  if (!variable.forall(Character.isLetter)) {
    throw new Exception(s"Variable(): malformed param: '$paramIn'")
  }

  var term = variable

  def display: Unit = { print("'" + variable + "'") }
}

class Numeral(paramIn: String) extends Atom {
  val numeral = paramIn.trim
  if (!numeral.forall(Character.isDigit)) {
    throw new Exception(s"Numeral(): malformed param: '$paramIn'")
  }

  var term = numeral

  def display: Unit = { print("'" + numeral + "'") }
}

object TermTools {
  def createTermTree(term: String): TermTree = {
    // This method is not intented to create Params
    if (isVariable(term)) return new Variable(term)
    if (isNumeral(term)) return new Numeral(term)
    // KeyLexer requires the first term be a key
    val lexer = new KeyLexer(term)
    val (token, strToken) = lexer.NextToken
    if (token == Token.Key)
      return new Key(term)
    throw new Exception("createTermTree: not recognized: " + strToken)
  }

  /*
  def createTermTree(term: String): TermTree =
  {
    // This method is not intented to create Params
    val lexer = new KeyLexer(term)
    val (token, strToken) = lexer.NextToken
    token match
    {
      case Token.Key => { return new Key(strToken) }
      case Token.Variable => { return new Variable(strToken) }
      case Token.Numeral => { return new Numeral(strToken) }
    }
    throw new Exception("createTermTree: not recognized: '"+ strToken +"'")
  }
   */

  def isVariable(s: String): Boolean = {
    s.forall(Character.isLetter)
  }
  def isNumeral(s: String): Boolean = {
    s.forall(Character.isDigit)
  }

}
