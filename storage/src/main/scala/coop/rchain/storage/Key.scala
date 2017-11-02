/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

// This class represents key as a tree of TermTrees where each inner
// node is a Key where the number of branches matches the arity of
// the key and the leaves are atoms.

package coop.rchain.storage

import scala.collection.mutable.{ArrayBuffer, LinkedHashSet}

class Key(keyIn: String) extends TermTree with Ordered[Key] {
  val term = keyIn.replaceAll("\\s+", "")
  protected[storage] val keyOriginal = keyIn

  if (term == null || term.isEmpty) {
    throw new RChainException("Key constructor got null or empty parameter")
  }

  protected[storage] val (name, params) = createParseKey(term)

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

    var lexToken = lexer.NextToken

    while (lexToken.token != Token.EndOfString && lexToken.token != Token.Error) {
      lexToken.token match {
        case Token.Key => {
          keyName = lexToken.tokenStr
        }
        case Token.LeftParen => {
          val keyParams = createParseTree(lexer)

          paramsArray += new Key(keyName, new Params(keyParams))
        }
        case Token.RightParen => {
          return paramsArray
        }
        case Token.Variable | Token.Constant => {
          paramsArray += TermTools.createTermTree(lexToken.tokenStr)
        }
        case Token.Comma => {}
        case _ => {
          throw new RChainException(
            "createParseTree(): lexer error (1): '" + lexToken.tokenStr + "'")
        }
      }

      lexToken = lexer.NextToken
    }

    if (lexToken.token == Token.Error) {
      throw new RChainException(
        "createParseTree(): lexer error (2): '" + lexToken.tokenStr + "'")
    }

    throw new RChainException("createParseTree: shouldn't get to end of method")
  }

  def createParseKey(key: String): (String, Params) = {
    var lexer = new KeyLexer(key)

    val firstLexToken = lexer.NextToken
    if (firstLexToken.token != Token.Key) {
      throw new RChainException("createParseTree: first token is not Key")
    }
    val keyName = firstLexToken.tokenStr

    val secondLexToken = lexer.NextToken
    if (secondLexToken.token != Token.LeftParen) {
      throw new RChainException("createParseTree: first token is not Key")
    }
    val paramsArray = createParseTree(lexer)
    (keyName, new Params(paramsArray))
  }

  // Return a list of lists where each inner list is a successful
  // unification with a particular key in the store.

  def unifyQuery(storage: Storage): LinkedHashSet[Array[Binding]] = {
    var bindings = LinkedHashSet[Array[Binding]]()

    val keyIter = storage.uniKeys
    while (keyIter.hasNext()) {
      val key = keyIter.next()
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
    if (term == key.term) { return 0 }

    if (arity > key.arity) { return 1 } else if (arity < key.arity) {
      return -1
    }

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
          if (term(i) < key.term(i)) {
            return -1
          } else if (term(i) > key.term(i)) {
            return 1
          }
        }
      }
    }

    assert(term.length != key.term.length)
    if (term.length > key.term.length) { return 1 }
    -1
  }

  def display: Unit = { print(term) }
}
