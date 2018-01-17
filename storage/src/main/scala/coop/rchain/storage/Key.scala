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
    var rightParenSeen = false

    var lexToken = lexer.nextToken

    while (lexToken.token != Token.EndOfString && lexToken.token != Token.Error
           && !rightParenSeen) {
      lexToken.token match {
        case Token.Key => {
          keyName = lexToken.tokenStr
        }
        case Token.LeftParen => {
          val keyParams = createParseTree(lexer)

          paramsArray += new Key(keyName, new Params(keyParams))
        }
        case Token.RightParen => {
          rightParenSeen = true
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

      if (!rightParenSeen) {
        lexToken = lexer.nextToken
      }
    }

    if (rightParenSeen || lexToken.token == Token.EndOfString) {
      paramsArray
    } else {
      if (lexToken.token == Token.Error) {
        throw new RChainException(
          "createParseTree(): lexer error (2): '" + lexToken.tokenStr + "'")
      }
      throw new RChainException(
        "createParseTree: shouldn't get to end of method")
    }
  }

  def createParseKey(key: String): (String, Params) = {
    var lexer = new KeyLexer(key)

    val firstLexToken = lexer.nextToken
    if (firstLexToken.token != Token.Key) {
      throw new RChainException("createParseTree: first token is not Key")
    }
    val keyName = firstLexToken.tokenStr

    val secondLexToken = lexer.nextToken
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

  // Compare two keys in a manner analagous to strcmp(key.term, this.term)
  // with these modifications:
  //   sort by: arity, letter, digit
  //
  // Arity is sorted such that f(1) will be ahead of f(1,1).
  // In unification, only keys of the same arity will be compared.
  // Predicates will smaller arity are sorted first for reasons
  // of readability in listing.
  //
  // Letters are sorted ahead of digits by fiat.

  def compare(key: Key): Int = {
    var returnVal = 0

    if (term == key.term) {
      0
    } else if (arity > key.arity) {
      1
    } else if (arity < key.arity) {
      -1
    } else {
      val length = math.min(term.length, key.term.length)
      var i = 0
      while (i < length && returnVal == 0) {
        if (term(i) != key.term(i)) {
          // check if term(i) is letter and key.term(i) is digit or vice-versa
          if (Character.isLetter(term(i))
              && Character.isDigit(key.term(i))) {
            returnVal = -1
          } else if (Character.isDigit(term(i))
                     && Character.isLetter(key.term(i))) {
            returnVal = 1
          } else {
            if (term(i) < key.term(i)) {
              returnVal = -1
            } else if (term(i) > key.term(i)) {
              returnVal = 1
            }
          }
        }
        i += 1
      }

      if (returnVal != 0) {
        returnVal
      } else {
        assert(term.length != key.term.length)
        if (term.length > key.term.length) {
          1
        } else {
          -1
        }
      }
    }
  }

  def display: Unit = { print(term) }
}
