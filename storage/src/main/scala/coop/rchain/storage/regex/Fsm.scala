/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain                          **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.storage.regex

import scala.collection.GenMap
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}

/**
  * A companion object for the Fsm class.
  */
object Fsm {

  /**
    * An FSM accepting nothing (not even the empty string). This is
    * demonstrates that this is possible, and is also extremely useful
    * in some situations
    */
  def NullFsm(alphabet: Set[Char]): Fsm =
    Fsm(alphabet,
        Set(0),
        0,
        Set(),
        Map(0 -> alphabet.map(c => c -> 0).toMap))

  /**
    * Return an FSM matching an empty string, "", only.
    */
  def EpsilonFsm(alphabet: Set[Char]): Fsm = Fsm(alphabet, Set(0), 0, Set(0), Map())

  /**
    * This is a surrogate symbol which you can use in your finite state machines
    * to represent "any symbol not in the official alphabet". For example, if your
    * state machine's alphabet is {"a", "b", "c", "d", fsm.anything_else}, then
    * you can pass "e" in as a symbol and it will be converted to
    * fsm.anything_else, then follow the appropriate transition.
    * Currently char code 0xE000 is used. This is a private-use-character, allowed for internal usages,
    * as stated in the Unicode standard: http://www.unicode.org/versions/Unicode5.2.0/ch16.pdf#G19635
    */
  def AnythingElse: Char = '\uE000'

  /**
    * In rare cases we need Fsm.AnythingElse to be the last symbol on the alphabet
    */
  private[this] def sortAlphabet(alphabet: Set[Char]): Seq[Char] =
    if (alphabet.contains(AnythingElse)) {
      alphabet.toList.sortBy(sym =>
        if (sym == Fsm.AnythingElse) Int.MaxValue else sym.toInt)
    } else {
      alphabet.toSeq
    }

  /**
    * Given the above conditions and instructions, crawl a new unknown FSM,
		* mapping its states, final states and transitions. Return the new FSM.
    * This is a pretty powerful procedure which could potentially go on
		* forever if you supply an evil version of follow().
    */
  private[regex] def crawl[T](alphabet: Set[Char],
                                 initial: T,
                                 isFinal: T => Boolean,
                                 follow: (T, Char) => Option[T]): Fsm = {
    val states = ArrayBuffer(initial)

    val finals = HashSet[Int]()
    val transitions = HashMap[Int, Map[Char, Int]]()

    var sortedAlphabet = sortAlphabet(alphabet)

    //iterate over a growing list
    var currentStateIdx = 0
    while (currentStateIdx < states.size) { //we can't use any kind of range loop or iterators here, since collection is growing
      val currentStates = states(currentStateIdx)
      if (isFinal(currentStates)) {
        finals += currentStateIdx //add to finals if needed
      }

      val currentStateMap = sortedAlphabet
        .flatMap(symbol =>
          follow(currentStates, symbol) match {
            case None => None
            case Some(nextState) => {
              var nextStateIdx = states.indexOf(nextState)
              if (nextStateIdx < 0) {
                nextStateIdx = states.size
                states += nextState
              }
              Some(symbol -> nextStateIdx)
            }
        })
        .toMap

      transitions += currentStateIdx -> currentStateMap
      currentStateIdx += 1
    }

    new Fsm(alphabet, states.indices.toSet, 0, finals.toSet, transitions.toMap)
  }

  /**
    * Crawl several FSMs in parallel, mapping the states of a larger meta-FSM.
    * To determine whether a state in the larger FSM is final, pass all of the
    * finality statuses (e.g. [True, False, False] to `test`.
    */
  def Parallel(finalityTest: Seq[Boolean] => Boolean, fsms: Seq[Fsm]): Fsm = {
    val alphabet = fsms.flatMap(m => m.alphabet).toSet

    val initial = fsms.zipWithIndex.map(x => x._2 -> x._1.initialState).toMap

    def follow(currentState: GenMap[Int, Int], symbol: Char): Option[GenMap[Int, Int]] = {
      val next = HashMap[Int, Int]()

      for ((fsm, fsmIdx) <- fsms.zipWithIndex.view) {
        val fsmState = currentState.get(fsmIdx)
        if (fsmState.isDefined) {
          val transition = fsm.transitions.get(fsmState.get)
          if (transition.isDefined) {
            val nextState = transition.get.get(symbol)
            if (nextState.isDefined) {
              next += fsmIdx -> nextState.get
            }
          }
        }
      }

      if (next.nonEmpty) {
        Some(next)
      } else {
        None
      }
    }

    def isFinal(fsmStates: GenMap[Int, Int]): Boolean = {
      val finalityStates = fsms.zipWithIndex
        .map(fsmAndIndex => {
          val (fsm, fsmIndex) = fsmAndIndex
          val fsmState = fsmStates.get(fsmIndex)
          fsmState.isDefined && fsm.finalStates.contains(fsmState.get)
        }).toList
      finalityTest(finalityStates)
    }

    crawl(alphabet, initial, isFinal, follow).Reduced
  }

  /**
    * Treat `fsms` as a collection of arbitrary FSMs and return the union FSM.
    * Can be used as `fsm1.union(fsm2, ...)` or `fsm.union(fsm1, ...)`. `fsms`
    * may be empty.
    */
  def Union(fsms: Fsm*): Fsm = Parallel(finalityStates => finalityStates.exists(x => x), fsms)

  /**
    * Intersection.
    * Take FSMs and AND them together. That is, return an FSM which
    * accepts any sequence of symbols that is accepted by both of the original
    * FSMs. Note that the set of strings recognised by the two FSMs undergoes
    * a set intersection operation.
    * Call using "fsm3 = fsm1 & fsm2"
    */
  def Intersection(fsms: Fsm*): Fsm = Parallel(finalityStates => finalityStates.forall(x => x), fsms)

  /**
    * Difference. Returns an FSM which recognises only the strings
    * recognised by the first FSM in the list, but none of the others.
    */
  def Difference(fsms: Fsm*): Fsm = Parallel(finalityStates => finalityStates.head && !finalityStates.drop(1).forall(x => x), fsms)

  /**
    * Treat `fsms` as a collection of sets of strings and compute the symmetric
    * difference of them all.
    */
  def SymmetricDifference(fsms: Fsm*): Fsm = Parallel(finalityStates => (finalityStates.count(x => x) & 1) == 1, fsms)

  /**
    * Concatenate arbitrarily many finite state machines together.
    */
  def Concatenate(fsms: Fsm*): Fsm = {
    val alphabet = fsms.flatMap(m => m.alphabet).toSet

    // Take a state in the numbered FSM and return a set containing it, plus
    // (if it's final) the first state from the next FSM, plus (if that's
    // final) the first state from the next but one FSM, plus...
    def connectAll(fsmIdx: Int, substate: Int): HashSet[(Int, Int)] = {
      val finalToInitialLinks = HashSet(fsmIdx -> substate)

      var currentFsmIdx = fsmIdx
      var currentState = substate
      //index magic below, be careful!
      while ((currentFsmIdx < fsms.size - 1) && fsms(currentFsmIdx).finalStates
               .contains(currentState)) {
        currentFsmIdx += 1
        currentState = fsms(currentFsmIdx).initialState
        finalToInitialLinks += currentFsmIdx -> currentState
      }
      finalToInitialLinks
    }

    //if we're in a final state of the final FSM, it's final
    //key: Index, value: fsms(i).currentState
    def isFinal(statesSet: HashSet[(Int, Int)]): Boolean = statesSet.exists(
      fsmIndexAndState => (fsmIndexAndState._1 == fsms.size - 1)
        && fsms(fsmIndexAndState._1).finalStates.contains(fsmIndexAndState._2))

    val initalStates = if (fsms.nonEmpty) {
      connectAll(0, fsms(0).initialState)
    } else {
      HashSet[(Int, Int)]()
    }

    // Follow the collection of states through all FSMs at once, jumping to the
    // next FSM if we reach the end of the current one
    // TODO: improve all follow() implementations to allow for dead metastates?
    def follow(currentStates: HashSet[(Int, Int)], currentSymbol: Char): Option[HashSet[(Int, Int)]] = {
      val nextStates = HashSet[(Int, Int)]()

      for ((fsmIndex, fsmState) <- currentStates) {
        val fsm = fsms(fsmIndex)
        val transition = fsm.transitions.get(fsmState)
        if (transition.isDefined) {
          val nextState = transition.get.get(currentSymbol)
          if (nextState.isDefined) {
            val changes = connectAll(fsmIndex, nextState.get)
            nextStates ++= changes
          }
        }
      }

      if (nextStates.isEmpty) {
        None
      } else {
        Some(nextStates)
      }
    }

    crawl(alphabet, initalStates, isFinal, follow).Reduced
  }
}

/**
  * A Finite State Machine or FSM has an alphabet and a set of states. At any
  * given moment, the FSM is in one state. When passed a symbol from the
  * alphabet, the FSM jumps to another state (or possibly the same state).
  * The 'transtions' map indicates where to jump.
  * One state is nominated as a starting state. Zero or more states are
  * nominated as final states. If, after consuming a string of symbols,
  * the FSM is in a final state, then it is said to "accept" the string.
  * This class also has some pretty powerful methods which allow FSMs to
  * be concatenated, alternated between, multiplied, looped (Kleene star
  * closure), intersected, and simplified.
  * The majority of these methods are available using operator overloads.
  */
case class Fsm(alphabet: Set[Char],
               states: Set[Int],
               initialState: Int,
               finalStates: Set[Int],
               transitions: Map[Int, Map[Char, Int]]) {
  require(alphabet != null, "alphabet can't be null")
  require(states != null, "alphabet can't be null")
  require(finalStates != null, "finals can't be null")
  require(transitions != null, "map can't be null")

  require(states.contains(initialState), "Initial state " + initialState + " must be one of states")
  require(finalStates.forall(fin => states.contains(fin)), "Final states must be a subset of states")

  for((mapEntry, stateTransitionEntry) <- transitions;
      (nextSymbol, nextState) <- stateTransitionEntry) {
    require(states.contains(mapEntry), "Map state " + mapEntry + " must be one of states")
    require(alphabet.contains(nextSymbol), "Transition symbol " + nextSymbol + " -> " + nextState + " must be one of alphabet")
    require(states.contains(nextState), "Transition state  " + nextSymbol + " -> " + nextState + " must be one of states")
  }

  /**
    * True if FSM alphabet contains Fsm.AnythingElse and thus can accept unknown characters
    */
  val HasAnythingElse: Boolean = alphabet.contains(Fsm.AnythingElse)

  /**
    * A state is "live" if a final state can be reached from it.
    */
  def IsLive(state: Int): Boolean = {
    val reachable = Queue(state)
    val checked = HashSet(state) //regexes usually have quite small number of states, sorted set should be faster in most cases

    while (reachable.nonEmpty) {
      val reachableState = reachable.dequeue()

      if (finalStates.contains(reachableState)) {
        return true
      }

      checked += reachableState

      transitions.get(reachableState) match {
        case Some(charToStateMap) =>
          charToStateMap.foreach(charToState => {
            val (_, transitionTargetState) = charToState
            if (!checked.contains(transitionTargetState)) {
              reachable += transitionTargetState
            }
          })
        case None =>
      }
    }

    false
  }

  /**
    * Test whether the present FSM accepts the supplied string (iterable of symbols).
    * Equivalently, consider `self` as a possibly-infinite set of
    * strings and test whether `string` is a member of it.
    * This is actually mainly used for unit testing purposes.
    * If `fsm.anything_else` is in your alphabet, then any symbol not in your
    * alphabet will be converted to `fsm.anything_else`
    */
  def Accepts(input: String): Boolean = {
    var currentState = initialState
    for (currentSymbol <- input) {
      val sym =
        if (HasAnythingElse & !alphabet.contains(currentSymbol))
          Fsm.AnythingElse
        else currentSymbol

      transitions.get(currentState) match {
        case None =>
          return false //Current state doesn't exist, FSM is Null or similar
        case Some(charToStateMap) => {
          charToStateMap.get(sym) match {
            case Some(nextState) => currentState = nextState
            case None =>
              return false //Missing transition = transition to dead state
          }
        }
      }
    }

    finalStates.contains(currentState)
  }

  /**
    * Compute the Brzozowski derivative of this FSM with respect to the input
    * string of symbols. <https://en.wikipedia.org/wiki/Brzozowski_derivative>
    * If any of the symbols are not members of the alphabet, that's a NoSuchElementException.
    * If you fall into oblivion, then the derivative is an FSM accepting no
    * strings.
    */
  def Derive(input: String): Fsm = {
    var currentState = initialState
    for (currentSymbol <- input) {
      val sym = if (alphabet.contains(currentSymbol)) {
        currentSymbol
      } else if (HasAnythingElse) {
        Fsm.AnythingElse
      } else {
        throw new NoSuchElementException(
          "Symbol '" + currentSymbol + "' is not in the source alphabet.")
      }

      transitions.get(currentState) match {
        case None => return Fsm.NullFsm(alphabet)
        case Some(charToStateMap) => {
          charToStateMap.get(sym) match {
            case Some(nextState) => currentState = nextState
            case None            => return Fsm.NullFsm(alphabet)
          }
        }
      }
    }
    //we've consumed the input, use the new location as the starting point.
    new Fsm(alphabet, states, currentState, finalStates, transitions)
  }

  /**
    * Return a new FSM such that for every string that self accepts (e.g.
    * "beer", the new FSM accepts the reversed string ("reeb").
    */
  def Reversed: Fsm = {
    //Reversed FSM uses the same alphabet
    val resAlphabet = alphabet
    //Start from a composite "state-set" consisting of all final states.
    //If there are no final states, this set is empty and we'll find that
    //no other states get generated.
    val resInitials = finalStates
    //Find every possible way to reach the current state-set
    //using this symbol.
    def follow(currentStates: Set[Int], currentSymbol: Char): Option[Set[Int]] = {
      val nextStates = (for {
        (transitionState, transitionMap) <- transitions
        currentState <- currentStates
        if transitionMap.get(currentSymbol).contains(currentState)
      } yield transitionState).toSet

      if (nextStates.isEmpty) {
        None
      } else {
        Some(nextStates)
      }
    }
    //A state-set is final if the initial state is in it.
    def isFinal(statesSet: Set[Int]): Boolean = statesSet.contains(initialState)
    //run crawl, and do not reduce() the result, since reduce() calls us in turn
    Fsm.crawl(resAlphabet, resInitials, isFinal, follow)
  }

  /**
    * Return a finite state machine which will accept any string NOT
    * accepted by self, and will not accept any string accepted by self.
    * This is more complicated if there are missing transitions, because the
    * missing "dead" state must now be reified.
    */
  def EverythingBut: Fsm = {
    def resInitial = initialState :: Nil

    def follow(currentState: List[Int], currentSymbol: Char): Option[List[Int]] = {
      var next = List[Int]()
      if (currentState.nonEmpty) {
        val transition = transitions.get(currentState.head)
        if (transition.isDefined) {
          val nextState = transition.get.get(currentSymbol)
          if (nextState.isDefined) {
            next = nextState.get :: Nil
          }
        }
      }
      Some(next)
    }

    //state is final unless the original was
    def isFinal(statesSet: List[Int]): Boolean =
      !(statesSet.nonEmpty && finalStates.contains(statesSet.head))

    Fsm.crawl(alphabet, resInitial, isFinal, follow).Reduced
  }

  /**
    * Generate strings (lists of symbols) that this FSM accepts. Since there may
    * be infinitely many of these we use a Stream[String] instead of constructing a
    * static list. Strings will be sorted in order of length and then lexically.
    * This procedure uses arbitrary amounts of memory but is very fast. There
    * may be more efficient ways to do this, that I haven't investigated yet.
    * You can use this in list comprehensions.
    */
  def Strings: Stream[String] = {
    // Many FSMs have "dead states". Once you reach a dead state, you can no
    // longer reach a final state. Since many strings may end up here, it's
    // advantageous to constrain our search to live states only.
    val livestates = states.filter(state => IsLive(state))

    // We store a list of tuples. Each tuple consists of an input string and the
    // state that this input string leads to. This means we don't have to run the
    // state machine from the very beginning every time we want to check a new
    // string.
    val pending = Queue[(String, Int)]()
    val pendingFinals = Queue[String]()
    var needInitial = true

    def nextStr: Option[String] =
      if (pendingFinals.nonEmpty) {
        Some(pendingFinals.dequeue())
      } else {
        if (needInitial) { //Initial entry (or possibly not, in which case this is a short one)
          needInitial = false
          val cstate = this.initialState
          if (livestates.contains(cstate)) {
            pending += "" -> cstate
            if (finalStates.contains(cstate)) {
              pendingFinals += ""
            }
          }
        }

        while (pendingFinals.isEmpty && pending.nonEmpty) {
          val (pendingString, pendingState) = pending.dequeue()

          val transition = transitions.get(pendingState)
          if (transition.isDefined) {
            for ((nextSymbol, nextState) <- transition.get) {
              val nextString = pendingString + nextSymbol

              if (livestates.contains(nextState)) {
                pending += nextString -> nextState
                if (finalStates.contains(nextState)) {
                  pendingFinals += nextString
                }
              }
            }
          }
        }

        if (pendingFinals.nonEmpty) {
          Some(pendingFinals.dequeue())
        } else {
          None
        }
      }

    def infStream: Stream[String] = {
      val str = nextStr
      if (str.isDefined) {
        str.get #:: infStream
      } else {
        Stream.empty[String]
      }
    }

    infStream
  }

  /**
    * Given an FSM and a multiplier, return the multiplied FSM.
    */
  def Times(multiplier: Int): Fsm = {
    require(multiplier >= 0, "Can't multiply an FSM by " + multiplier)

    def initial = HashSet(this.initialState -> 0) //currentState -> iteration

    def follow(crawlState: HashSet[(Int, Int)],
               symbol: Char): Option[HashSet[(Int, Int)]] = {
      val next = HashSet[(Int, Int)]()

      crawlState.foreach(fsmStateAndIteration => {
        val fsmState = fsmStateAndIteration._1
        val iteration = fsmStateAndIteration._2

        if (iteration < multiplier) {
          val transition = transitions.get(fsmState)
          if (transition.nonEmpty) {
            val subState = transition.get.get(symbol)
            if (subState.nonEmpty) {
              next += subState.get -> iteration
              //final of self? merge with initial on next iteration
              if (finalStates.contains(subState.get)) {
                next += initialState -> (iteration + 1)
              }
            }
          }
        }
      })

      if (next.nonEmpty) {
        Some(next)
      } else {
        None
      }
    }

    //If the initial state is final then multiplying doesn't alter that
    def isFinal(crawlState: HashSet[(Int, Int)]): Boolean =
      crawlState.exists(fsmStateAndIteration =>
        (fsmStateAndIteration._1 == initialState)
          && (finalStates
            .contains(initialState) || (fsmStateAndIteration._2 == multiplier)))

    Fsm.crawl(alphabet, initial, isFinal, follow).Reduced
  }

  /**
    * If the present FSM accepts X, returns an FSM accepting X* (i.e. 0 ormore Xes).
    * This is NOT as simple as naively connecting the final states back to the initial state:
    * see (b*ab)* for example.
    */
  def Star: Fsm = {
    def follow(subStates: HashSet[Int], symbol: Char): Option[HashSet[Int]] = {
      val next = HashSet[Int]()

      for (subState <- subStates) {
        val transition = transitions.get(subState)
        if (transition.nonEmpty) {
          val nextState = transition.get.get(symbol)
          if (nextState.nonEmpty) {
            next += nextState.get
          }
        }
        //If one of our substates is final, then we can also consider
        //transitions from the initial state of the original FSM.
        if (finalStates.contains(subState)) {
          val transition = transitions.get(initialState)
          if (transition.isDefined) {
            val nextState = transition.get.get(symbol)
            if (nextState.nonEmpty) {
              next += nextState.get
            }
          }
        }
      }

      if (next.nonEmpty) {
        Some(next)
      } else {
        None
      }
    }

    def isFinal(subStates: HashSet[Int]): Boolean = subStates.exists(subState => finalStates.contains(subState))

    Fsm.crawl(alphabet, HashSet(initialState), isFinal, follow) | Fsm.EpsilonFsm(alphabet)
  }

  /**
    * Consider the FSM as a set of strings and return the cardinality of thatset
    * - Some[Int], or None if there are infinitely many
    */
  def Cardinality: Option[Int] = {
    val stateToCount = HashMap[Int, Option[Int]]()

    def getNumStrings(state: Int): Option[Int] =
      if (IsLive(state)) {
        val stateCount = stateToCount.get(state)

        if (stateCount.nonEmpty) {
          if (stateCount.get.isEmpty)
            return None //fail fast = we trapped into infinite recursion
          stateCount.get
        } else {
          stateToCount += state -> None //None means "computing right now..."
          var n = 0
          if (finalStates.contains(state)) {
            n += 1
          }

          var transition = transitions.get(state)
          if (transition.isDefined) {
            for ((_, nextState) <- transition.get) {
              val countForNextState = getNumStrings(nextState)
              if (countForNextState.isEmpty) {
                return None //fail fast - we trapped into infinite recursion there
              }
              n += countForNextState.get //yup, recursion here
            }
          }

          stateToCount += state -> Some(n)
          Some(n)
        }
      } else {
        stateToCount += state -> Some(0)
        Some(0)
      }

    getNumStrings(initialState)
  }

  /**
    * Treat `this` and `that` as sets of strings and see if `this` is a subset of `that`... `this` recognises no strings which `that` doesn't.
    */
  def IsSubset(that: Fsm): Boolean = (this - that).IsEmpty

  /**
    * Same as IsSubset, and also checks that FSMs aren't equal.
    */
  def IsStrictSubset(that: Fsm): Boolean = (this - that).IsEmpty && (this != that)

  /**
    * Treat `this` and `that` as sets of strings and see if `this` is a superset of `that`...
    */
  def IsSuperset(that: Fsm): Boolean = (that - this).IsEmpty

  /**
    * Same as IsSuperset, and also checks that FSMs aren't equal.
    */
  def IsStrictSuperset(that: Fsm): Boolean = (that - this).IsEmpty && (this != that)

  /**
    * Shortcut for the Cardinality method
    */
  def Length: Option[Int] = Cardinality

  /**
    * Symmetric difference. Returns an FSM which recognises only the strings
    * recognised by `this` or `that` but not both.
    */
  def SymmetricDifference(that: Fsm): Fsm = Fsm.SymmetricDifference(this, that)

  /**
    * Difference. Returns an FSM which recognises only the strings
    * recognised by the first FSM in the list, but none of the others.
    */
  def Difference(that: Fsm): Fsm = Fsm.Difference(this, that)

  /**
    * Treat the FSMs as sets of strings and return the intersection of those
    * sets in the form of a new FSM.
    */
  def Intersection(that: Fsm): Fsm = Fsm.Intersection(this, that)

  def Union(that: Fsm): Fsm = Fsm.Union(this, that)

  /**
    * Concatenate arbitrarily many finite state machines together.
    */
  def Concatenate(that: Fsm): Fsm = Fsm.Concatenate(this, that)

  /**
    * Concatenate arbitrarily many finite state machines together.
    */
  def Concatenate(others: Fsm*): Fsm = Fsm.Concatenate(this :: others.toList: _*)

  /**
    * Treat `this` and `other` as sets of strings and see if they are disjoint
    */
  def IsDisjoint(that: Fsm): Boolean = (this & that).IsEmpty

  /**
    * An FSM is empty if it recognises no strings. An FSM may be arbitrarily
    * complicated and have arbitrarily many final states while still recognising
    * no strings because those final states may all be inaccessible from the
    * initial state. Equally, an FSM may be non-empty despite having an empty
    * alphabet if the initial state is final.
    */
  def IsEmpty: Boolean = !IsLive(initialState)

  /**
    * Two FSMs are considered equivalent if they recognise the same strings.
    * Or, to put it another way, if their symmetric difference recognises no
    * strings.
    */
  def Equivalent(that: Fsm): Boolean = (this ^ that).IsEmpty

  /**
    * Two FSMs are considered different if they have a non-empty symmetric difference.
    */
  def Different(that: Fsm): Boolean = !(this ^ that).IsEmpty

  /**
    * Returns a new FSM with empty and duplicate transitions removed
    */
  def Reduced: Fsm = Reversed.Reversed

  /**
    * An alias for the 'Accepts' method
    */
  def Contains(str: String): Boolean = Accepts(str)

  //region Operators

  /**
    * You can use `fsm1 == fsm2` to determine whether two FSMs recognise the same strings.
    */
  def ==(that: Fsm): Boolean = Equivalent(that)

  /**
    * Two FSMs are considered different if they have a non-empty symmetric difference.
    */
  def !=(that: Fsm): Boolean = Different(that)

  /** Treat `this` and `that` as sets of strings and see if `this` is a subset
    of `that`... `this` recognises no strings which `that` doesn't. */
  def <=(that: Fsm): Boolean = IsSubset(that)

  /**
    * Treat `this` and `that` as sets of strings and see if `this` is a strict subset of `that`.
    */
  def <(that: Fsm): Boolean = IsStrictSubset(that)

  /** Treat `this` and `that` as sets of strings and see if `this` is a superset
    of `that`... `that` recognises no strings which `this` doesn't.
    */
  def >=(that: Fsm): Boolean = IsSuperset(that)

  /**
    * Treat `this` and `that` as sets of strings and see if `this` is a strict superset of `that`.
    */
  def >(that: Fsm): Boolean = IsStrictSuperset(that)

  /**
    * Symmetric difference. Returns an FSM which recognises only the strings recognised by `this` or `that` but not both.
    */
  def ^(that: Fsm): Fsm = SymmetricDifference(that)

  /**
    * Treat the FSMs as sets of strings and return the intersection of those
    * sets in the form of a new FSM.
    */
  def &(that: Fsm): Fsm = Intersection(that)

  /**
    * Alternation.
    * Return a finite state machine which accepts any sequence of symbols
    * that is accepted by either self or other. Note that the set of strings
    * recognised by the two FSMs undergoes a set union.
    */
  def |(that: Fsm): Fsm = Union(that)

  /**
    * Concatenate 'this' and 'that' state machines.
    */
  def +(that: Fsm): Fsm = Concatenate(that)

  /**
    * Difference. Returns an FSM which recognises only the strings
    * recognised by the first FSM in the list, but none of the others.
    */
  def -(that: Fsm): Fsm = Difference(that)

  /**
    * Given an FSM and a multiplier, return the multiplied FSM.
    */
  def *(multiplier: Int): Fsm = Times(multiplier)

  //endregion
}
