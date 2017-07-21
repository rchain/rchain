/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore


// Keys are like Prolog terms, where the predicate
// name is all letters and each parameter is
// all digits or all letters.

class Key(key: String)
{
  private val _parsed = parse(key)
  val _name = _parsed._1
  val _params = _parsed._2
  val _arity = _parsed._2.length
  var _key = _name + "("
  for (i <- 0 until _params.length-1)
    _key += _params(i).param + ","
  _key += _params(_params.length-1).param + ")"

  def this(name: String, params: String*)
  {
    this(name + "(" + params.mkString(",") + ")")
  }

  def parse(key: String): (String,Array[Param])  = 
  {
    // need to check the form of key
    val name = key.slice(0, key.indexOfSlice("(")).trim
    if (name.length == 0 || name.length == key.length || !allLetters(name))
      { throw new Exception(s"Key.Parse(): malformed (1): $key") }
    val paramsOriginal =
      key.slice(key.indexOfSlice("(")+1, key.length-1).split(",")
    val params = new Array[Param](paramsOriginal.length)

    var i = 0
    for (p <- paramsOriginal)
    {
      val param = p.trim
      if ((allLetters(param) || allDigits(param))
          // disallow the same <letters> appearing more than once
          && (allDigits(param) || !params.contains(param)))
      {
        // Why doesn't this work?
        // params(i) = MakeParam(param)
        // error: not found: value MakeParam

        if (allLetters(param))
          params(i) = new Variable(param)
        else if (allDigits(param))
          params(i) = new Value(param)
        else
          throw new Exception(s"Key.Parse(): unexpected input: '$param'")

        i += 1
      }
      else { throw new Exception(s"Key.Parse(): malformed (2): $key, $param") }
    }

    (name, params)
  }

  def allLetters(s:String): Boolean =
  {
    s.forall(Character.isLetter)
  }
  def allDigits(s:String): Boolean =
  {
    s.forall(Character.isDigit)
  }
  def display: Unit =
  {
    println(_key)
  }
}

