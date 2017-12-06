/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

// KeyLexer takes a Key, expressed as a string, and
// sequentializes its lexical parts.

package coop.rchain.storage

object Token extends Enumeration {
  val LeftParen = Value("(")
  val RightParen = Value(")")
  val Comma = Value(",")
  val Key = Value("Key")
  val Variable = Value("Variable")
  val Constant = Value("Constant")
  val EndOfString = Value("EndOfString")
  val Error = Value("Error")
}

class LexToken(tokenIn: Token.Value, tokenStrIn: String) {
  val token = tokenIn
  val tokenStr = tokenStrIn
}

// Current assumption is that what is lexed must be on one line.
// Relax assumption later.

class KeyLexer(lineIn: String) {
  if (lineIn == null || lineIn.isEmpty)
    throw new RChainException("KeyLexer: string is null or empty")
  if (!Character.isLetter(lineIn(0)))
    throw new RChainException(
      "KeyLexer: '" + lineIn + "' must start with a key")

  // Check that parentheses nest correctly and all have partners.
  protected[storage] var depth = 0
  for (i <- 0 until lineIn.length) {
    if (lineIn(i) == '(') depth += 1
    else if (lineIn(i) == ')') depth -= 1
    if (depth < 0)
      throw new RChainException("KeyLexer: malformed (1): '" + lineIn + "'")
  }
  if (depth != 0)
    throw new RChainException("KeyLexer: malformed (2): '" + lineIn + "'")

  protected[storage] val line = lineIn
  protected[storage] var i = 0 // index into line

  def nextToken(): LexToken = {
    var returnVal: Option[LexToken] = None

    val tokenStr = new StringBuilder
    var isLetters = false
    var isDigits = false
    val iOriginal = i

    if (line.length <= i) {
      new LexToken(Token.EndOfString, "")
    } else {
      var c = line(i)

      if (c == '(') {
        if (0 < i) {
          val prev = line(i - 1)
          if (prev == ')')
            throw new RChainException(
              "KeyLexer.nextToken(): malformed (1): '"
                + line + "', " + i)
        }

        i += 1
        returnVal = Some(new LexToken(Token.LeftParen, "("))
      } else if (c == ')') {
        if (0 < i) {
          val prev = line(i - 1)
          if (prev == '(' || prev == ',')
            throw new RChainException(
              "KeyLexer.nextToken(): malformed (2): '"
                + line + "', " + i)
        }

        i += 1
        returnVal = Some(new LexToken(Token.RightParen, ")"))
      } else if (c == ',') {
        if (0 < i) {
          val prev = line(i - 1)
          if (prev == '(' || prev == ',')
            throw new RChainException(
              "KeyLexer.nextToken(): malformed (3): '"
                + line + "', " + i)
        }

        i += 1
        returnVal = Some(new LexToken(Token.Comma, ","))
      }

      if (!returnVal.isDefined) {
        var endOfString = false

        while (i < line.length && c != '(' && c != ')' && c != ',') {
          if (Character.isLetter(c)) {
            if (isDigits)
              throw new RChainException(
                "KeyLexer.nextToken(): malformed (4): '"
                  + line + "', " + i)
            isLetters = true
            tokenStr ++= line(i).toString
          } else if (Character.isDigit(c)) {
            if (isLetters)
              throw new RChainException(
                "KeyLexer.nextToken(): malformed (5): '"
                  + line + "', " + i)
            isDigits = true
            tokenStr ++= line(i).toString
          } else
            throw new RChainException(
              "KeyLexer.nextToken(): malformed (6): '"
                + line + "', " + i)

          i += 1
          c = line(i)
          assert(isLetters != isDigits)
        }

        if (isDigits) {
          returnVal = Some(new LexToken(Token.Constant, tokenStr.toString))
        } else {
          if (isLetters) {
            if (iOriginal == 0 || (line(i) == '('
                && (line(iOriginal - 1) == '(' || line(iOriginal - 1) == ',')))
              returnVal = Some(new LexToken(Token.Key, tokenStr.toString))
            else
              returnVal = Some(new LexToken(Token.Variable, tokenStr.toString))
          }

          if (!returnVal.isDefined) {
            throw new RChainException(
              "KeyLexer.nextToken(): malformed (7): '"
                + line + "', " + i)
          }
        }
      }
      returnVal.get
    }
  }
}
