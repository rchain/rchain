/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package KeyValueStore

import scala.collection.mutable._


// A Query is an all-letters predicate followed by parameters

class Query(val query: String) extends Key(query) with Param
{
  var _query = _key

  def this(name: String, params: String*)
  {
    this(name + "(" + params.mkString(",") + ")")
  }

  def param: String = { _query }
  def isVariable: Boolean = { false }
  def isValue: Boolean = { false }
  def isTerm: Boolean = { true }

  def execute(keyValueStore: KeyValueStore)
    : LinkedHashSet[Array[(Param,Param)]] = 
  {
    val matches = LinkedHashSet[Array[(Param,Param)]]()

    for((k, v) <- keyValueStore._keyValueStore)
    {
      val queryName = k.slice(0, k.indexOfSlice("("))
      // keyParams is a string because a string is a key to KeyValueStore
      val keyParams = k.slice(k.indexOfSlice("(")+1, k.length-1).split(",")
      if (_params.length == keyParams.length && queryName == _name)
      {
        // Test if query values match key values. For example,
        // query A(3,x) matches key value A(3,4) but not A(2,4)
        var valuesMatch = true
        var queryParamIsVariable = true
        for (i <- 0 until _params.length)
        {
          val keyParamStr = keyParams(i)
          var keyParam: Param = makeParam(keyParamStr)
          val queryParam = _params(i)
          if (queryParam.isValue && keyParam.isValue
            && queryParam.param != keyParam.param)
          {
            valuesMatch = false
          }
        }
 
        // The only case we are excluding is two values in
        // the same position is both key and query that do not match.
        if (valuesMatch)
        {
          var bindings = new Array[(Param,Param)](_params.length)
          var bindingIndex = 0

          for (i <- 0 until _params.length)
          {
            val keyParamStr = keyParams(i)
            var keyParam: Param = makeParam(keyParamStr)
            val queryParam = _params(i)

            val duple = (queryParam, keyParam)
            bindings(bindingIndex) = duple
            bindingIndex += 1
          }
          matches += bindings
        }
      }
    }
    matches
  }
}

object QueryUtils
{
  def queryResultsToArrayString(query: Query,
    queryResult: LinkedHashSet[Array[(Param,Param)]],
    store: KeyValueStore): (String, Array[String]) =
  {
    val queryName = query._name
    var results = new Array[String](queryResult.size)

    var bindinsIndex = 0
    for (bindings <- queryResult)
    {
      var bindingsStr = "{"
      var params = ""
      for (i <- 0 until bindings.size)
      {
        val binding = bindings(i)
        if (binding._1.isValue && binding._2.isValue)
        {
          assert(binding._1.param == binding._2.param)
          bindingsStr += binding._1.param + ","
        }
        else if (binding._1.isVariable && binding._2.isVariable
          && binding._1.param == binding._2.param)
        {
          bindingsStr += binding._1.param + ","
        }
        else
          bindingsStr += binding._1.param + ":" + binding._2.param + ","
        params += binding._2.param + ","
      }
      bindingsStr = bindingsStr.slice(0, bindingsStr.length-1)
      bindingsStr += "} -> "

      params = params.slice(0, params.length-1)

      bindingsStr += store.get(queryName + "(" + params + ")").toString

      results(bindinsIndex) = bindingsStr
      bindinsIndex += 1
    }

    (query._query, results)
  }
}

