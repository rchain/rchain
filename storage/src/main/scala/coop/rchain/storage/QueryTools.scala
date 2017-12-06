/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

// This object provides tools used in unifying a query with a
// storage.  A query is a key that is unified with the keys in
// the store.

package coop.rchain.storage

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, LinkedHashSet}

class Binding(queryP: TermTree, keyP: TermTree) {
  if (queryP == null || keyP == null) {
    throw new RChainException("Binding constructor has bad parameter")
  }

  protected[storage] val queryParam = queryP
  protected[storage] val keyParam = keyP

  override def toString: String = {
    "(" + queryParam.term + "," + keyParam.term + ")"
  }
  def display: Unit = {
    print(this.toString)
  }
}

object QueryTools {

  // Unify the query and key parameters.
  // The return value has a Boolean that expresses whether the unification
  // succeeded or not and if it did results of the unification are in the array.

  def unifyParams(
      queryParams: ArrayBuffer[TermTree],
      keyParams: ArrayBuffer[TermTree]): (Boolean, Array[Binding]) = {
    var returnVal = true

    if (queryParams.length != keyParams.length) {
      (false, Array[Binding]())
    } else {
      var bindings = new ArrayBuffer[Binding]()

      var i = 0
      while (i < queryParams.length && returnVal) {
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
              for (b <- bindingsToAdd) {
                bindings += b
              }
            } else {
              returnVal = false
            }
          } else {
            returnVal = false
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
            returnVal = false
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
            returnVal = false
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
            returnVal = false
          }
        } else {
          returnVal = false
        }
        i += 1
      }
      if (returnVal) {
        (true, bindings.toArray)
      } else {
        (false, Array[Binding]())
      }
    }
  }

  // Given the key, return a new key with all the bindings substituted

  protected[storage] def createKeySubstition(key: Key,
                                             bindings: Array[Binding],
                                             bindingIndex: Int = 0): Key = {
    val keyName = key.name
    val keyParams = key.params

    val subs = createParamsSubstition(keyParams, bindings, bindingIndex)
    new Key(keyName, subs)
  }

  protected[storage] def createParamsSubstition(params: Params,
                                                bindings: Array[Binding],
                                                bindingIndexIn: Int): Params = {
    if (bindings.length == 0) {
      throw new RChainException(
        "createParamsSubstitution(): empty array parameter")
    }

    var returnParams = new ArrayBuffer[TermTree]
    var bindingIndex = bindingIndexIn

    for (param <- params.params) {
      param match {
        case _: Key => {
          val key = param.asInstanceOf[Key]
          val keyName = key.name
          val keyParams = key.params

          val subs = createParamsSubstition(keyParams, bindings, bindingIndex)
          bindingIndex += keyParams.length

          returnParams += new Key(keyName, subs)
        }
        case _: Variable | _: Constant => {
          assert(bindingIndex < bindings.length)
          val bindingOption = bindings(bindingIndex)
          bindingIndex += 1
          val binding = bindingOption
          val queryParam = binding.queryParam
          val keyParam = binding.keyParam
          if (queryParam.term == param.term) {
            returnParams += TermTools.createTermTree(keyParam.term)
          }
        }
        case _ => {
          throw new RChainException(
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

  def queryResultsToArrayString(query: Key,
                                queryResult: LinkedHashSet[Array[Binding]],
                                store: Storage): Unification = {
    var standardRep = new ArrayBuffer[String]()

    // Each element of queryResult is an array of bindings that
    // corresponds to a term in the store.
    for (bindingsArray <- queryResult) {
      var standardBindingsStr = "[queryVars:{"
      val uniRep = divideUniResults(bindingsArray)
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
      val valuesOption = store.getStrings(keySub.term)
      if (valuesOption.isDefined) {
        val values = stringArrayToValuesRepString(valuesOption.get)
        standardBindingsStr += values
        standardRep += standardBindingsStr
      }
    }
    assert(standardRep.size == queryResult.size)

    standardRep.toArray
  }

  def queryResultsToKeys(query: Key,
                         queryResult: LinkedHashSet[Array[Binding]],
                         store: Storage): Array[Key] = {
    var standardRep = new ArrayBuffer[Key]()

    // Each element of queryResult is an array of bindings that
    // corresponds to a term in the store.
    for (bindingsArray <- queryResult) {
      val uniRep = divideUniResults(bindingsArray)
      val keySub = createKeySubstition(query, bindingsArray)
      val valuesOption = store.getStrings(keySub.term)
      if (valuesOption.isDefined) {
        standardRep += keySub
      }
    }
    assert(standardRep.size == queryResult.size)

    standardRep.toArray
  }

  // This method supports the standard representation of unification
  // results.
  // Divide unification results into two arrays: one containing
  // the bindings where the variable is in the query and one
  // where the variable is in the key.

  def divideUniResults(uniResults: Array[Binding]): UniRep = {
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

  def stringArrayToValuesRepString(valuesArray: Array[String]): String = {
    val valuesBuf = new mutable.StringBuilder()
    valuesBuf ++= "["
    for (i <- 0 until valuesArray.size - 1) {
      val value = valuesArray(i) + ","
      valuesBuf ++= value
    }
    valuesBuf ++= (valuesArray(valuesArray.size - 1) + "]")
    valuesBuf.toString
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

  type Unification = Array[String]
}
