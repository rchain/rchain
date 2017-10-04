/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.kv

import scala.collection.mutable.ArrayBuffer

trait TermTree {
  var term: String // entire term, including params for a key

  def isKey: Boolean = this.isInstanceOf[Key]
  def isVariable: Boolean = this.isInstanceOf[Variable]
  def isConstant: Boolean = this.isInstanceOf[Constant]

  def typeIs: String = {
    if (this.isKey) return ("Key")
    if (this.isVariable) return ("Variable")
    if (this.isConstant) return ("Constant")
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

  def display: Unit = print(term)
}

trait Atom extends TermTree {}

class Variable(param: String) extends Atom {
  val variable = param.trim

  if (!TermTools.isVariable(variable))
    throw new Exception(s"Variable(): malformed param: '$param'")

  var term = variable

  def display: Unit = print("'" + variable + "'")
}

// constants are numerals or names

class Constant(param: String) extends Atom {
  val constant = param.trim

  if (!TermTools.isConstant(constant))
    throw new Exception(s"Constant(): malformed param: '$param'")

  var term = constant

  def display: Unit = print("'" + constant + "'")
}

object TermTools {
  def createTermTree(term: String): TermTree = {
    // This method is not intended to create Params
    if (isVariable(term)) return new Variable(term)
    if (isConstant(term)) return new Constant(term)
    // KeyLexer requires the first term be a key
    assert(term.indexOf("(") < term.indexOf(")"))
    val lexer = new KeyLexer(term)
    val lexToken = lexer.NextToken
    if (lexToken.token == Token.Key)
      return new Key(term)
    throw new Exception("createTermTree: not recognized: " + lexToken.tokenStr)
  }

  def isVariable(s: String): Boolean = {
    if (s == null || s.isEmpty)
      throw new Exception("isVariable argument is null or empty")
    s(0).isUpper
  }
  def isConstant(s: String): Boolean = {
    if (s == null || s.isEmpty)
      throw new Exception("isVariable argument is null or empty")
    s(0).isLower || s(0).isDigit
  }
}
