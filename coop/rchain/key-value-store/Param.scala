/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore


trait Param
{
  def param: String

  def makeParam(param: String): Param =
  {
    if (isLetters(param))
      return new Variable(param)
    else if (isDigits(param))
      return new Value(param)

    throw new Exception(s"Param.makeParam(): unexpected input: '$param'")
  }

  def isVariable: Boolean
  def isValue: Boolean
  def isTerm: Boolean

  def isLetters(s:String): Boolean =
  {
    s.forall(Character.isLetter)
  }
  def isDigits(s:String): Boolean =
  {
    s.forall(Character.isDigit)
  }
}

class Variable(paramIn: String) extends Param
{
  val _param = paramIn.trim
  if (!_param.forall(Character.isLetter))
    { throw new Exception(s"Variable(): malformed param: '$param'") }

  def param: String = { _param }
  def isVariable: Boolean = { true }
  def isValue: Boolean = { false }
  def isTerm: Boolean = { false }
}

class Value(paramIn: String) extends Param
{
  val _param = paramIn.trim
  if (!_param.forall(Character.isDigit))
    { throw new Exception(s"Value(): malformed param: '$param'") }

  def param: String = { _param }
  def isVariable: Boolean = { false }
  def isValue: Boolean = { true }
  def isTerm: Boolean = { false }
}

