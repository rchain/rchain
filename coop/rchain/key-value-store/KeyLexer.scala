/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

object Token extends Enumeration {
  val LeftParen = Value("(")
  val RightParen = Value(")")
  val Comma = Value(",")
  val Key = Value("Key")
  val Variable = Value("Variable")
  val Numeral = Value("Numeral")
  val EndOfLine = Value("EndOfLine")
  val Error = Value("Error")
}

// Current assumption is that what is lexed must be on one line.
// Relax assumption later.

class KeyLexer(line: String) {
  if (line.length == 0)
    throw new Exception("KeyLexer: string is empty")
  if (!Character.isLetter(line(0)))
    throw new Exception(
      "KeyLexer: '"
        + line + "' must start with a letter")

  // Check that parentheses nest correctly and all have partners.
  var depth = 0
  for (i <- 0 until line.length) {
    if (line(i) == '(') depth += 1
    else if (line(i) == ')') depth -= 1
    if (depth < 0)
      throw new Exception("KeyLexer: malformed (1): '" + line + "'")
  }
  if (depth != 0)
    throw new Exception("KeyLexer: malformed (2): '" + line + "'")

  val _line = line
  var _i = 0 // index into _line

  def PeekNextTokenIsEndOfLine: Boolean = {
    _line.length <= _i
  }

  def NextToken(): (Token.Value, String) = {
    val tokenStr = new StringBuilder
    var isLetters = false
    var isDigits = false
    val iOriginal = _i

    if (_line.length <= _i)
      return (Token.EndOfLine, "")

    var c = _line(_i)

    if (c == '(') {
      if (0 < _i) {
        val prev = _line(_i - 1)
        if (prev == ')')
          throw new Exception(
            "KeyLexer.NextToken(): malformed (1): '"
              + _line + "', " + _i)
      }

      _i += 1
      return (Token.LeftParen, "(")
    } else if (c == ')') {
      if (0 < _i) {
        val prev = _line(_i - 1)
        if (prev == '(' || prev == ',')
          throw new Exception(
            "KeyLexer.NextToken(): malformed (2): '"
              + _line + "', " + _i)
      }

      _i += 1
      return (Token.RightParen, ")")
    } else if (c == ',') {
      if (0 < _i) {
        val prev = _line(_i - 1)
        if (prev == '(' || prev == ',')
          throw new Exception(
            "KeyLexer.NextToken(): malformed (3): '"
              + _line + "', " + _i)
      }

      _i += 1
      return (Token.Comma, ",")
    }

    while (_i < _line.length && c != '(' && c != ')' && c != ',') {
      c match {
        case _ if Character.isLetter(c) => {
          if (isDigits)
            throw new Exception(
              "KeyLexer.NextToken(): malformed (4): '"
                + _line + "', " + _i)
          isLetters = true
          tokenStr ++= _line(_i).toString
        }

        case _ if Character.isDigit(c) => {
          if (isLetters)
            throw new Exception(
              "KeyLexer.NextToken(): malformed (5): '"
                + _line + "', " + _i)
          isDigits = true
          tokenStr ++= _line(_i).toString
        }

        case _ =>
          throw new Exception(
            "KeyLexer.NextToken(): malformed (6): '"
              + _line + "', " + _i)
      }

      _i += 1
      c = _line(_i)
      assert(isLetters != isDigits)
    }

    if (isDigits)
      return (Token.Numeral, tokenStr.toString)

    if (isLetters) {
      if (iOriginal == 0 || (_line(_i) == '('
          && (_line(iOriginal - 1) == '(' || _line(iOriginal - 1) == ',')))
        return (Token.Key, tokenStr.toString)
      else
        return (Token.Variable, tokenStr.toString)
    }

    throw new Exception(
      "KeyLexer.NextToken(): malformed (7): '"
        + _line + "', " + _i)
    (Token.Error, "")
  }
}
