/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable._

// A query is a key that is unified against a key store.

class Binding(queryP: Atom, keyP: Atom) {
  if (queryP == null || keyP == null) {
    // throw
  }

  val queryParam = queryP
  val keyParam = keyP
}

object QueryTools {
  def unifyParams(
      queryParams: ArrayBuffer[TermTree],
      keyParams: ArrayBuffer[TermTree]): (Boolean, Array[Binding]) = {
    if (queryParams.length != keyParams.length)
      return (false, Array[Binding]())

    var bindings = new ArrayBuffer[Binding]()

    for (i <- 0 until queryParams.length) {
      val queryParam = queryParams(i)
      val keyParam = keyParams(i)

      if (queryParam.isKey && keyParam.isKey) {
        // wrap in try
        val queryPred = queryParam.asInstanceOf[Key]
        val keyPred = keyParam.asInstanceOf[Key]
        if (queryPred.name == keyPred.name
            && queryPred.arity == keyPred.arity) {
          val (success, bindingsToAdd) =
            unifyParams(queryPred.params.params, keyPred.params.params)
          if (success)
            for (b <- bindingsToAdd)
              bindings += b
        } else {
          return (false, Array[Binding]())
        }
      } else if (queryParam.isVariable) {
        if (keyParam.isKey) {
          // recursive unification here
          return (false, Array[Binding]())
        } else if (keyParam.isVariable) {
          // wrap in try
          val queryVar = queryParam.asInstanceOf[Variable]
          val keyVar = keyParam.asInstanceOf[Variable]
          val binding = new Binding(queryVar, keyVar)
          bindings += binding
        } else if (keyParam.isNumeral) {
          // wrap in try
          val queryVar = queryParam.asInstanceOf[Variable]
          val keyNum = keyParam.asInstanceOf[Numeral]
          val binding = new Binding(queryVar, keyNum)
          bindings += binding
        } else {
          return (false, Array[Binding]())
        }
      } else if (queryParam.isNumeral) {
        if (keyParam.isVariable) {
          // wrap in try
          val queryNum = queryParam.asInstanceOf[Numeral]
          val keyVar = keyParam.asInstanceOf[Variable]
          // set variable to numeral
          val binding = new Binding(queryNum, keyVar)
          bindings += binding
        } else if (keyParam.isNumeral
                   && queryParam.term == keyParam.term) {
          // wrap in try
          val queryNum = queryParam.asInstanceOf[Numeral]
          val keyNum = keyParam.asInstanceOf[Numeral]
          // match
          val binding = new Binding(queryNum, keyNum)
          bindings += binding
        } else {
          return (false, Array[Binding]())
        }
      } else {
        return (false, Array[Binding]())
      }
    }
    val returnArray = QueryTools.ArrayBufferToArray(bindings)
    (returnArray.size == queryParams.length, returnArray)
  }

  def createKeySubstition(key: Key, bindings: Array[Binding]): Key = {
    val keyName = key.name
    val keyParams = key.params

    val subs = createParamsSubstition(keyParams, bindings)

    new Key(keyName, subs)
  }

  def createParamsSubstition(params: Params,
                             bindingsIn: Array[Binding]): Params = {
    assert(0 < bindingsIn.length)

    // create a copy of bindingsIn so it can be changed
    // without changing the actual bindingsIn
    var bindings = new Array[Binding](bindingsIn.length)
    for (i <- 0 until bindingsIn.length)
      bindings(i) = bindingsIn(i)

    var returnParams = new ArrayBuffer[TermTree]

    var bindingsIndex = 0
    for (param <- params.params) {
      param.termType match {
        case TermType.Key => {
          // wrap in try
          val key = param.asInstanceOf[Key]
          val keyName = key.name
          val keyParams = key.params

          val subs = createParamsSubstition(keyParams, bindings)

          returnParams += new Key(keyName, subs)

          bindings = bindings.slice(subs.length, bindings.length)
          bindingsIndex = 0
        }
        case TermType.Variable | TermType.Numeral => {
          val binding = bindings(bindingsIndex)
          val queryParam = binding.queryParam
          val keyParam = binding.keyParam

          if (queryParam.term == param.term) {
            returnParams += TermTools.makeAtom(keyParam.term)
            bindingsIndex += 1
          }
        }
        case TermType.Error => {
          throw new Exception(
            "createParamsSubstition: '" + param.term
              + "' not found in provided bindings")
        }
      }
    }

    new Params(returnParams)
  }

  def queryResultsToArrayString(
      query: Key,
      queryResult: LinkedHashSet[Array[Binding]],
      store: KeyValueStore): (String, Array[String]) = {
    var results = new Array[String](queryResult.size)
    var bindingsIndex = 0
    for (bindingsArray <- queryResult) {
      var bindingsStr = "{"
      for (binding <- bindingsArray) {
        if (binding.queryParam.isNumeral && binding.keyParam.isNumeral) {
          assert(binding.queryParam.term == binding.keyParam.term)
          bindingsStr += binding.queryParam.term + ","
        } else if (binding.queryParam.isVariable && binding.keyParam.isVariable
                   && binding.queryParam.term == binding.keyParam.term) {
          bindingsStr += binding.queryParam.term + ","
        } else
          bindingsStr += binding.queryParam.term + ":" + binding.keyParam.term + ","
      }
      bindingsStr = bindingsStr.slice(0, bindingsStr.length - 1)
      bindingsStr += "} -> "

      val keySub = createKeySubstition(query, bindingsArray)
      /* this error is incredibly irritating
      try {
        val values = store.get(keySub)
        bindingsStr += values.toString
      }
      catch {
        // compile error: value isDefinedAt is not a member of Nothing
        return (query.term, Array[String]())
      }
       */

      val values = store.getOrNull(keySub)
      if (values != null) {
        bindingsStr += values.toString
        results(bindingsIndex) = bindingsStr
      }

      bindingsIndex += 1
    }
    (query.term, results)
  }

  /* tried to be generic
  def ArrayBufferToArray[T](ab: ArrayBuffer[T]) : Array[T] =
  {
    // compiler error: cannot find class tag for element type T
    var a = new Array[T](ab.size)
    var i = 0
    for (x <- ab)
    {
      a(i) = x
      i += 1
    }
    a
  }
   */
  def ArrayBufferToArray(ab: ArrayBuffer[Binding]): Array[Binding] = {
    var a = new Array[Binding](ab.size)
    var i = 0
    for (x <- ab) {
      a(i) = x
      i += 1
    }
    a
  }

}
