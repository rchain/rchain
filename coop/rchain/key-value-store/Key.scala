/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable._

// Represent key as a tree of TermTrees where each inner node
// is a Key where the number of branches matches the arity of
// the key and the leaves are atoms.

class Key(keyIn: String) extends TermTree with Ordered[Key] {
  var term = keyIn.replaceAll("\\s+", "")
  val keyOriginal = keyIn

  assert(term != "")

  val (name, params) = createParseKey(term)

  val arity = params.length

  def this(name: String, params: String*) {
    this(name + "(" + params.mkString(",") + ")")
  }

  def this(name: String, params: Params) {
    this(name + params.term)
  }

  def createParseTree(lexer: KeyLexer): ArrayBuffer[TermTree] = {
    var keyName = ""
    var paramsArray = new ArrayBuffer[TermTree]()

    var (token, tokenStr) = lexer.NextToken

    while (token != Token.EndOfString && token != Token.Error) {
      token match {
        case Token.Key => {
          keyName = tokenStr
        }
        case Token.LeftParen => {
          val keyParams = createParseTree(lexer)

          paramsArray += new Key(keyName, new Params(keyParams))
        }
        case Token.RightParen => {
          return paramsArray
        }
        case Token.Variable | Token.Numeral => {
          paramsArray += TermTools.createTermTree(tokenStr)
        }
        case Token.Comma => {}
        case _ => {
          throw new Exception(
            "createParseTree(): lexer error (1): '" + tokenStr + "'")
        }
      }

      val tokenResult = lexer.NextToken
      token = tokenResult._1
      tokenStr = tokenResult._2
    }

    if (token == Token.Error) {
      throw new Exception(
        "createParseTree(): lexer error (2): '" + tokenStr + "'")
    }

    throw new Exception("createParseTree: shouldn't get to end of method")
  }

  def createParseKey(key: String): (String, Params) = {
    var lexer = new KeyLexer(key)

    val firstTokenResult = lexer.NextToken
    val firstToken = firstTokenResult._1
    if (firstToken != Token.Key)
      throw new Exception("createParseTree: first token is not Key")
    val keyName = firstTokenResult._2

    val secondTokenResult = lexer.NextToken
    val secondToken = secondTokenResult._1
    if (secondToken != Token.LeftParen)
      throw new Exception("createParseTree: first token is not Key")
    val paramsArray = createParseTree(lexer)
    (keyName, new Params(paramsArray))
  }

  def unifyQuery(keyValueStore: KeyValueStore): LinkedHashSet[Array[Binding]] = {
    var bindings = LinkedHashSet[Array[Binding]]()

    for ((key, _) <- keyValueStore.keyValueStore) {
      val keyName = key.name
      val keyParams = key.params
      if (name == keyName && arity == key.arity) {
        val (success, bindingsToAdd) =
          QueryTools.unifyParams(params.params, keyParams.params)
        if (success) {
          bindings += bindingsToAdd
        }
      }
    }
    bindings
  }

  // sort by: arity, character, digit, other
  def compare(key: Key): Int = {
    if (term == key.term) return 0

    if (arity > key.arity) return 1
    else if (arity < key.arity) return -1

    val length = math.min(term.length, key.term.length)
    for (i <- 0 until length) {
      if (term(i) != key.term(i)) {
        if (Character.isLetter(term(i))
            && Character.isDigit(key.term(i))) {
          return -1
        } else if (Character.isDigit(term(i))
                   && Character.isLetter(key.term(i))) {
          return 1
        } else {
          if (term(i) < key.term(i))
            return -1
          else if (term(i) > key.term(i))
            return 1
        }
      }
    }

    if (term.length > key.term.length) return 1
    else return -1
  }

  def display: Unit = { print(term) }
}
