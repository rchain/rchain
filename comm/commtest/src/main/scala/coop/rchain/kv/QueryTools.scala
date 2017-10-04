/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.kv

import scala.collection.mutable._

// A query is a key that is unified against a key in the store.

class Binding(queryP: TermTree, keyP: TermTree) {
  if (queryP == null || keyP == null)
    throw new Exception("Binding constructor has bad parameter")

  val queryParam = queryP
  val keyParam = keyP

  override def toString: String =
    "(" + queryParam.term + "," + keyParam.term + ")"
  def display: Unit =
    print(this.toString)
}

object QueryTools {

  // Unify the query and key parameters.
  // The return value has a Boolean that expresses whether the unification
  // succeeded or not and if it did results of the unification are in the array.

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
        val queryPred = queryParam.asInstanceOf[Key]
        val keyPred = keyParam.asInstanceOf[Key]
        if (queryPred.name == keyPred.name
            && queryPred.arity == keyPred.arity) {
          val (success, bindingsToAdd) =
            unifyParams(queryPred.params.params, keyPred.params.params)
          if (success) {
            for (b <- bindingsToAdd)
              bindings += b
          } else
            return (false, Array[Binding]())
        } else {
          return (false, Array[Binding]())
        }
      } else if (queryParam.isVariable) {
        if (keyParam.isKey) {
          // variable is bound to key
          val queryVar = queryParam.asInstanceOf[Variable]
          val keyPred = keyParam.asInstanceOf[Key]
          val binding = new Binding(queryVar, keyPred)

          bindings += binding
        } else if (keyParam.isVariable) {
          val queryVar = queryParam.asInstanceOf[Variable]
          val keyVar = keyParam.asInstanceOf[Variable]
          val binding = new Binding(queryVar, keyVar)
          bindings += binding
        } else if (keyParam.isConstant) {
          val queryVar = queryParam.asInstanceOf[Variable]
          val keyConst = keyParam.asInstanceOf[Constant]
          val binding = new Binding(queryVar, keyConst)
          bindings += binding
        } else {
          return (false, Array[Binding]())
        }
      } else if (queryParam.isConstant) {
        if (keyParam.isKey) {
          /* constants do not unify with Keys
          val queryConst = queryParam.asInstanceOf[Constant]
          val keyPred = keyParam.asInstanceOf[Key]
          // set variable to constant
          val binding = new Binding(queryConst, keyPred)
          bindings += binding
           */
          return (false, Array[Binding]())
        } else if (keyParam.isVariable) {
          val queryConst = queryParam.asInstanceOf[Constant]
          val keyVar = keyParam.asInstanceOf[Variable]
          // set variable to constant
          val binding = new Binding(queryConst, keyVar)

          bindings += binding
        } else if (keyParam.isConstant
                   && queryParam.term == keyParam.term) {
          val queryConst = queryParam.asInstanceOf[Constant]
          val keyConst = keyParam.asInstanceOf[Constant]
          val binding = new Binding(queryConst, keyConst)
          bindings += binding
        } else {
          return (false, Array[Binding]())
        }
      } else {
        return (false, Array[Binding]())
      }
    }
    (true, bindings.toArray)
  }

  // Given the key, return a new key with all the bindings substituted

  def createKeySubstition(key: Key, bindings: Array[Binding]): Key = {
    val keyName = key.name
    val keyParams = key.params

    val subs = createParamsSubstition(keyParams, bindings)
    new Key(keyName, subs)
  }

  def createParamsSubstition(params: Params,
                             bindingsIn: Array[Binding]): Params = {
    if (bindingsIn.length == 0)
      throw new Exception("createParamsSubstitution(): empty array parameter")

    // create a copy of bindingsIn so it can be changed
    // without changing the actual bindingsIn
    var bindings = new Array[Binding](bindingsIn.length)
    for (i <- 0 until bindingsIn.length)
      bindings(i) = bindingsIn(i)

    var returnParams = new ArrayBuffer[TermTree]

    var bindingsIndex = 0
    for (param <- params.params) {
      param match {
        case _: Key => {
          val key = param.asInstanceOf[Key]
          val keyName = key.name
          val keyParams = key.params

          val subs = createParamsSubstition(keyParams, bindings)

          returnParams += new Key(keyName, subs)

          bindings = bindings.slice(subs.length, bindings.length)
          bindingsIndex = 0
        }
        case _: Variable | _: Constant => {
          val binding = bindings(bindingsIndex)
          bindingsIndex += 1
          val queryParam = binding.queryParam
          val keyParam = binding.keyParam
          if (queryParam.term == param.term)
            returnParams += TermTools.createTermTree(keyParam.term)
        }
        case _ => {
          throw new Exception(
            "createParamsSubstition: '" + param.term
              + "' not found in provided bindings")
        }
      }
    }
    new Params(returnParams)
  }

  // The first input parameter is a query.  The second parameter contains
  // an Array[Binding] for each key in the store that unifies with the query
  // against the third parameter.
  //
  // The output is two representations of the unification.

  def queryResultsToArrayString(query: Key,
                                queryResult: LinkedHashSet[Array[Binding]],
                                store: KeyValueStore): TwoUnifications = {
    var standardRep = new ArrayBuffer[String]()
    var myRep = new ArrayBuffer[String]()

    // Each element of queryResult is an array of bindings that
    // corresponds to a term in the store.
    for (bindingsArray <- queryResult) {
      var myBindingsStr = "{"
      for (binding <- bindingsArray) {
        if (binding.queryParam.isConstant
            && binding.keyParam.isConstant) {
          assert(binding.queryParam.term == binding.keyParam.term)
          myBindingsStr += binding.queryParam.term + ","
        } else if (binding.queryParam.isVariable
                   && binding.keyParam.isVariable
                   && binding.queryParam.term == binding.keyParam.term) {
          myBindingsStr += binding.queryParam.term + ","
        } else {
          myBindingsStr += binding.queryParam.term + ":" + binding.keyParam.term + ","
        }
      }
      myBindingsStr = myBindingsStr.slice(0, myBindingsStr.length - 1)
      myBindingsStr += "} -> "

      var standardBindingsStr = "[queryVars:{"
      val uniRep = DivideUnificationResults(bindingsArray)
      if (0 < uniRep.queryVars.length) {
        for (binding <- uniRep.queryVars) {
          standardBindingsStr += binding.queryParam.term + ":" + binding.keyParam.term + ","
        }
        standardBindingsStr =
          standardBindingsStr.slice(0, standardBindingsStr.length - 1)
      }

      standardBindingsStr += "},keyVars:{"
      if (0 < uniRep.keyVars.length) {
        for (binding <- uniRep.keyVars) {
          standardBindingsStr += binding.queryParam.term + ":" + binding.keyParam.term + ","
        }
        standardBindingsStr =
          standardBindingsStr.slice(0, standardBindingsStr.length - 1)
      }
      standardBindingsStr += "}] -> "

      val keySub = createKeySubstition(query, bindingsArray)
      val values = store.get(keySub)
      if (values != null) {
        standardBindingsStr += values.toString
        standardRep += standardBindingsStr
        myBindingsStr += values.toString
        myRep += myBindingsStr
      }
    }
    assert(myRep.size == queryResult.size)
    assert(standardRep.size == queryResult.size)

    new TwoUnifications(standardRep.toArray, myRep.toArray)
  }

  // This method supports the standard representation of unification
  // results.
  // Divide unification results into two arrays: one containing
  // the bindings where the variable is in the query and one
  // where the variable is in the key.

  def DivideUnificationResults(uniResults: Array[Binding]): UniRep = {
    val varInQuery = new ArrayBuffer[Binding]()
    val varInKey = new ArrayBuffer[Binding]()

    for (binding <- uniResults) {
      if (binding.queryParam.isVariable && binding.keyParam.isVariable
          && binding.queryParam.term != binding.keyParam.term) {
        varInQuery += binding
        varInKey += new Binding(binding.keyParam, binding.queryParam)
      } else if (binding.queryParam.isVariable && !binding.keyParam.isVariable) {
        varInQuery += binding

      } else if (binding.keyParam.isVariable && !binding.queryParam.isVariable) {
        varInKey += new Binding(binding.keyParam, binding.queryParam)
      } else {
        // If you're here, the query and key variables match and are not
        // included in the results.
      }
    }
    new UniRep(varInQuery.toArray, varInKey.toArray)
  }

  // UniRep is used to express the standard representation of a unification
  // of a key and a query.
  //
  // queryVars represents how to transform the query into the matching key.
  // keyVars represents how to transform the matching key into the query.

  class UniRep(queryVarsIn: Array[Binding], keyVarsIn: Array[Binding]) {
    val queryVars = queryVarsIn
    val keyVars = keyVarsIn
  }
}

// See the comments in Main.scala about the uniRep variable.

class TwoUnifications(standardRepresentation: Array[String],
                      nonRepresentation: Array[String]) {
  val standard = standardRepresentation
  val non = nonRepresentation // standard
}
