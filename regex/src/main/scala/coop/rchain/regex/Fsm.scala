//package coop.rchain.regex
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//import scala.util.{Failure, Success, Try}
//
///**
//  * A companion object for the Fsm class.
//  */
//@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
//object Fsm {
//
//  /**
//    * An FSM accepting nothing (not even the empty string). This
//    * demonstrates that this is possible, and is also extremely useful
//    * in some situations
//    */
//  def nullFsm(alphabet: Set[Char]): Fsm =
//    Fsm(alphabet, Set(0), 0, Set(), Map(0 -> alphabet.map(_ -> 0).toMap))
//
//  /**
//    * Return an FSM matching an empty string, "", only.
//    */
//  def epsilonFsm(alphabet: Set[Char]): Fsm =
//    Fsm(alphabet, Set(0), 0, Set(0), Map())
//
//  /**
//    * This is a surrogate symbol which you can use in your finite state machines
//    * to represent "any symbol not in the official alphabet". For example, if your
//    * state machine's alphabet is {"a", "b", "c", "d", Fsm.AnythingElse}, then
//    * you can pass "e" in as a symbol and it will be converted to
//    * Fsm.AnythingElse, then follow the appropriate transition.
//    * Currently char code 0xE000 is used. This is a private-use-character, allowed for internal usages,
//    * as stated in the Unicode standard: http://www.unicode.org/versions/Unicode5.2.0/ch16.pdf#G19635
//    */
//  val anythingElse: Char = '\uE000'
//
//  /**
//    * In rare cases we need Fsm.AnythingElse to be the last symbol on the alphabet
//    */
//  private[this] def sortAlphabet(alphabet: Set[Char]): Seq[Char] =
//    if (alphabet.contains(anythingElse)) {
//      alphabet.toList.sortBy(sym => if (sym == anythingElse) Int.MaxValue else sym.toInt)
//    } else {
//      alphabet.toSeq
//    }
//
//  /**
//    * Given the above conditions and instructions, crawl a new unknown FSM,
//    * mapping its states, final states and transitions. Return the new FSM.
//    * This is a pretty powerful procedure which could potentially go on
//    * forever if you supply an evil version of follow().
//    */
//  private[regex] def crawl[T](
//      alphabet: Set[Char],
//      initial: T,
//      isFinal: T => Boolean,
//      follow: (T, Char) => Option[T]
//  ): Fsm = {
//    //actual type of the 'states' is deducted from 'initial' => ArrayBuffer[T]
//    val states      = mutable.ArrayBuffer(initial)
//    val transitions = mutable.Map[Int, Map[Char, Int]]()
//
//    val sortedAlphabet = sortAlphabet(alphabet)
//    //we can't use any kind of range loop or iterators here, since collection is growing
//    @tailrec
//    def iterate(currentStateIdx: Int = 0, currentFinals: Set[Int] = Set()): Set[Int] = {
//      val currentState = states(currentStateIdx)
//      //add to finals if needed
//      val nextFinals = if (isFinal(currentState)) {
//        currentFinals + currentStateIdx
//      } else {
//        currentFinals
//      }
//
//      val currentStateMap = sortedAlphabet
//        .flatMap(
//          symbol =>
//            follow(currentState, symbol).map(nextState => {
//              val nextStateIdx = states.indexOf(nextState)
//              symbol -> (if (nextStateIdx < 0) {
//                           states += nextState
//                           states.size - 1
//                         } else {
//                           nextStateIdx
//                         })
//            })
//        )
//        .toMap
//
//      transitions += currentStateIdx -> currentStateMap
//
//      if (currentStateIdx + 1 < states.size)
//        iterate(currentStateIdx + 1, nextFinals)
//      else
//        nextFinals
//    }
//
//    val iterateFinals = iterate()
//    Fsm(alphabet, states.indices.toSet, 0, iterateFinals, transitions.toMap)
//  }
//
//  /**
//    * Crawl several FSMs in parallel, mapping the states of a larger meta-FSM.
//    * To determine whether a state in the larger FSM is final, pass all of the
//    * finality statuses (e.g. [True, False, False] to `test`.
//    */
//  def parallel(finalityTest: Seq[Boolean] => Boolean, fsms: Seq[Fsm]): Fsm = {
//    val alphabet = fsms.flatMap(_.alphabet).toSet
//
//    val initial = fsms.zipWithIndex.map {
//      case (fsm, fsmIndex) => fsmIndex -> fsm.initialState
//    }.toMap
//
//    def follow(currentState: Map[Int, Int], symbol: Char): Option[Map[Int, Int]] = {
//      val next = fsms.zipWithIndex.flatMap {
//        case (fsm, fsmIdx) =>
//          currentState
//            .get(fsmIdx)
//            .flatMap(
//              fsmState =>
//                fsm
//                  .nextState(fsmState, symbol)
//                  .map(nextState => fsmIdx -> nextState)
//            )
//      }.toMap
//      Some(next).filter(_.nonEmpty)
//    }
//
//    def isFinal(fsmStates: Map[Int, Int]): Boolean = {
//      val finalityStates = fsms.zipWithIndex.map {
//        case (fsm, fsmIndex) =>
//          val fsmState = fsmStates.get(fsmIndex)
//          fsmState.isDefined && fsm.finalStates.contains(fsmState.get)
//      }.toList
//      finalityTest(finalityStates)
//    }
//
//    crawl(alphabet, initial, isFinal, follow).reduced
//  }
//
//  /**
//    * Treat `fsms` as a collection of arbitrary FSMs and return the union FSM.
//    * Can be used as `fsm1.union(fsm2, ...)` or `fsm.union(fsm1, ...)`. `fsms`
//    * may be empty.
//    */
//  def union(fsms: Fsm*): Fsm =
//    parallel(finalityStates => finalityStates.exists(x => x), fsms)
//
//  /**
//    * Intersection.
//    * Take FSMs and AND them together. That is, return an FSM which
//    * accepts any sequence of symbols that is accepted by both of the original
//    * FSMs. Note that the set of strings recognised by the two FSMs undergoes
//    * a set intersection operation.
//    * Call using "fsm3 = fsm1 & fsm2"
//    */
//  def intersection(fsms: Fsm*): Fsm =
//    parallel(finalityStates => finalityStates.forall(x => x), fsms)
//
//  /**
//    * Difference. Returns an FSM which recognises only the strings
//    * recognised by the first FSM in the list, but none of the others.
//    */
//  def difference(fsms: Fsm*): Fsm =
//    parallel(finalityStates => finalityStates.head && !finalityStates.drop(1).forall(x => x), fsms)
//
//  /**
//    * Treat `fsms` as a collection of sets of strings and compute the symmetric
//    * difference of them all.
//    */
//  def symmetricDifference(fsms: Fsm*): Fsm =
//    parallel(finalityStates => (finalityStates.count(x => x) & 1) == 1, fsms)
//
//  /**
//    * Concatenate arbitrarily many finite state machines together.
//    */
//  def concatenate(fsms: Fsm*): Fsm = {
//    val alphabet = fsms.flatMap(_.alphabet).toSet
//
//    // Take a state in the numbered FSM and return a set containing it, plus
//    // (if it's final) the first state from the next FSM, plus (if that's
//    // final) the first state from the next but one FSM, plus...
//    def connectAll(fsmIdx: Int, subState: Int): Set[(Int, Int)] = {
//      @tailrec
//      def updateIndexes(
//          currentFsmIdx: Int,
//          currentState: Int,
//          currentSet: Set[(Int, Int)]
//      ): Set[(Int, Int)] =
//        if ((currentFsmIdx < fsms.size - 1)
//            && fsms(currentFsmIdx).finalStates.contains(currentState)) {
//          val nextFsmIdx = currentFsmIdx + 1
//          val nextState  = fsms(nextFsmIdx).initialState
//          updateIndexes(nextFsmIdx, nextState, currentSet + (nextFsmIdx -> nextState))
//        } else {
//          currentSet
//        }
//
//      updateIndexes(fsmIdx, subState, Set(fsmIdx -> subState))
//    }
//
//    //if we're in a final state of the final FSM, it's final
//    //key: Index, value: fsms(i).currentState
//    def isFinal(statesSet: Set[(Int, Int)]): Boolean =
//      statesSet.exists {
//        case (fsmIndex, fsmState) =>
//          (fsmIndex == fsms.size - 1) && fsms(fsmIndex).finalStates
//            .contains(fsmState)
//      }
//
//    val initalStates = if (fsms.nonEmpty) {
//      connectAll(0, fsms(0).initialState)
//    } else {
//      Set[(Int, Int)]()
//    }
//
//    // Follow the collection of states through all FSMs at once, jumping to the
//    // next FSM if we reach the end of the current one
//    // TODO: improve all follow() implementations to allow for dead metastates?
//    def follow(currentStates: Set[(Int, Int)], currentSymbol: Char): Option[Set[(Int, Int)]] = {
//      val nextStates = currentStates.flatMap {
//        case (fsmIndex, fsmState) =>
//          fsms(fsmIndex)
//            .nextState(fsmState, currentSymbol)
//            .map(nextState => connectAll(fsmIndex, nextState))
//      }.flatten
//      Some(nextStates).filter(_.nonEmpty)
//    }
//
//    crawl(alphabet, initalStates, isFinal, follow).reduced
//  }
//}
//
///**
//  * A Finite State Machine or FSM has an alphabet and a set of states. At any
//  * given moment, the FSM is in one state. When passed a symbol from the
//  * alphabet, the FSM jumps to another state (or possibly the same state).
//  * The 'transtions' map indicates where to jump.
//  * One state is nominated as a starting state. Zero or more states are
//  * nominated as final states. If, after consuming a string of symbols,
//  * the FSM is in a final state, then it is said to "accept" the string.
//  * This class also has some pretty powerful methods which allow FSMs to
//  * be concatenated, alternated between, multiplied, looped (Kleene star
//  * closure), intersected, and simplified.
//  * The majority of these methods are available using operator overloads.
//  */
//@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
//final case class Fsm(
//    alphabet: Set[Char],
//    states: Set[Int],
//    initialState: Int,
//    finalStates: Set[Int],
//    transitions: Map[Int, Map[Char, Int]]
//) {
//  require(alphabet != null, "alphabet can't be null")
//  require(states != null, "alphabet can't be null")
//  require(finalStates != null, "finals can't be null")
//  require(transitions != null, "map can't be null")
//
//  require(states.contains(initialState), "Initial state " + initialState + " must be one of states")
//  require(
//    finalStates.forall(fin => states.contains(fin)),
//    "Final states must be a subset of states"
//  )
//
//  for {
//    (mapEntry, stateTransitionEntry) <- transitions
//    (nextSymbol, nextState)          <- stateTransitionEntry
//  } {
//    require(states.contains(mapEntry), "Map state " + mapEntry + " must be one of states")
//    require(
//      alphabet.contains(nextSymbol),
//      "Transition symbol " + nextSymbol + " -> " + nextState + " must be one of alphabet"
//    )
//    require(
//      states.contains(nextState),
//      "Transition state  " + nextSymbol + " -> " + nextState + " must be one of states"
//    )
//  }
//
//  /**
//    * True if FSM alphabet contains Fsm.AnythingElse and thus can accept unknown characters
//    */
//  val hasAnythingElse: Boolean = alphabet.contains(Fsm.anythingElse)
//
//  /**
//    * A state is "live" if a final state can be reached from it.
//    */
//  @SuppressWarnings(Array("org.wartremover.warts.Return"))
//  def isLive(state: Int): Boolean = {
//    val reachable = mutable.Queue(state)
//    val checked   = mutable.Set(state)
//    //can't use any functional iterator here (like reachable.exists), since queue is modified
//    while (reachable.nonEmpty) {
//      val reachableState = reachable.dequeue()
//
//      if (finalStates.contains(reachableState)) {
//        //we got the result, no more iterations needed, return immediately
//        return true
//      }
//
//      checked += reachableState
//      reachable ++= transitions
//        .get(reachableState)
//        .toList
//        .flatMap(
//          charToStateMap =>
//            charToStateMap.values
//              .filter(transitionTargetState => !checked.contains(transitionTargetState))
//        )
//    }
//
//    false
//  }
//
//  /**
//    * Test whether the present FSM accepts the supplied string (iterable of symbols).
//    * Equivalently, consider `self` as a possibly-infinite set of
//    * strings and test whether `string` is a member of it.
//    * This is actually mainly used for unit testing purposes.
//    * If `Fsm.AnythingElse` is in your alphabet, then any symbol not in your
//    * alphabet will be converted to `Fsm.AnythingElse`
//    */
//  @SuppressWarnings(Array("org.wartremover.warts.Return", "org.wartremover.warts.Var"))
//  def accepts(input: String): Boolean = {
//    var currentState = initialState
//    for (currentSymbol <- input) {
//      val sym = if (hasAnythingElse & !alphabet.contains(currentSymbol)) {
//        Fsm.anythingElse
//      } else {
//        currentSymbol
//      }
//
//      nextState(currentState, sym) match {
//        case None =>
//          //Current state doesn't exist, transition to dead state, FSM is Null or similar
//          return false
//        case Some(nextState) => currentState = nextState
//      }
//    }
//
//    finalStates.contains(currentState)
//  }
//
//  /**
//    * Compute the Brzozowski derivative of this FSM with respect to the input
//    * string of symbols. <https://en.wikipedia.org/wiki/Brzozowski_derivative>
//    * If any of the symbols are not members of the alphabet, that's a NoSuchElementException.
//    * If you fall into oblivion, then the derivative is an FSM accepting no
//    * strings.
//    */
//  @SuppressWarnings(Array("org.wartremover.warts.Return", "org.wartremover.warts.Var"))
//  def derive(input: String): Try[Fsm] = {
//    var currentState = initialState
//    for (currentSymbol <- input) {
//      val sym = if (alphabet.contains(currentSymbol)) {
//        currentSymbol
//      } else if (hasAnythingElse) {
//        Fsm.anythingElse
//      } else {
//        return Failure(
//          new NoSuchElementException(
//            "Symbol '" + currentSymbol + "' is not in the source alphabet."
//          )
//        )
//      }
//
//      nextState(currentState, sym) match {
//        case Some(nextState) => currentState = nextState
//        case None            => return Success(Fsm.nullFsm(alphabet))
//      }
//    }
//    //we've consumed the input, use the new location as the starting point.
//    Success(Fsm(alphabet, states, currentState, finalStates, transitions))
//  }
//
//  /**
//    * Return a new FSM such that for every string that self accepts (e.g.
//    * "beer", the new FSM accepts the reversed string ("reeb").
//    */
//  def reversed: Fsm = {
//    //Reversed FSM uses the same alphabet
//    val resAlphabet = alphabet
//    //Start from a composite "state-set" consisting of all final states.
//    //If there are no final states, this set is empty and we'll find that
//    //no other states get generated.
//    val resInitials = finalStates
//    //Find every possible way to reach the current state-set
//    //using this symbol.
//    def follow(currentStates: Set[Int], currentSymbol: Char): Option[Set[Int]] = {
//      val nextStates = (for {
//        (transitionState, transitionMap) <- transitions
//        currentState                     <- currentStates
//        if transitionMap.get(currentSymbol).contains(currentState)
//      } yield transitionState).toSet
//
//      Some(nextStates).filter(_.nonEmpty)
//    }
//    //A state-set is final if the initial state is in it.
//    def isFinal(statesSet: Set[Int]): Boolean = statesSet.contains(initialState)
//    //run crawl, and do not reduce() the result, since reduce() calls us in turn
//    Fsm.crawl(resAlphabet, resInitials, isFinal, follow)
//  }
//
//  /**
//    * Return a finite state machine which will accept any string NOT
//    * accepted by self, and will not accept any string accepted by self.
//    * This is more complicated if there are missing transitions, because the
//    * missing "dead" state must now be reified.
//    */
//  def everythingBut: Fsm = {
//    def resInitial = initialState :: Nil
//
//    def follow(currentState: List[Int], currentSymbol: Char): Option[List[Int]] =
//      Some(
//        currentState.headOption
//          .flatMap(headState => nextState(headState, currentSymbol))
//          .toList
//      )
//
//    //state is final unless the original was
//    def isFinal(statesSet: List[Int]): Boolean =
//      !statesSet.headOption.exists(x => finalStates.contains(x))
//    //!(statesSet.nonEmpty && finalStates.contains(statesSet.head))
//
//    Fsm.crawl(alphabet, resInitial, isFinal, follow).reduced
//  }
//
//  /**
//    * Generate strings (lists of symbols) that this FSM accepts. Since there may
//    * be infinitely many of these we use a LazyList[String] instead of constructing a
//    * static list. Strings will be sorted in order of length and then lexically.
//    * This procedure uses arbitrary amounts of memory but is very fast. There
//    * may be more efficient ways to do this, that I haven't investigated yet.
//    * You can use this in list comprehensions.
//    */
//  @SuppressWarnings(Array("org.wartremover.warts.Var"))
//  def strings: LazyList[String] = {
//    // Many FSMs have "dead states". Once you reach a dead state, you can no
//    // longer reach a final state. Since many strings may end up here, it's
//    // advantageous to constrain our search to live states only.
//    val livestates = states.filter(state => isLive(state))
//
//    // We store a list of tuples. Each tuple consists of an input string and the
//    // state that this input string leads to. This means we don't have to run the
//    // state machine from the very beginning every time we want to check a new
//    // string.
//    val pending       = mutable.Queue[(String, Int)]()
//    val pendingFinals = mutable.Queue[String]()
//    var needInitial   = true
//
//    def nextStr: Option[String] =
//      if (pendingFinals.nonEmpty) {
//        Some(pendingFinals.dequeue())
//      } else {
//        //Initial entry (or possibly not, in which case this is a short one)
//        if (needInitial) {
//          needInitial = false
//          val cstate = this.initialState
//          if (livestates.contains(cstate)) {
//            pending += "" -> cstate
//            if (finalStates.contains(cstate)) {
//              pendingFinals += ""
//            }
//          }
//        }
//
//        while (pendingFinals.isEmpty && pending.nonEmpty) {
//          val (pendingString, pendingState) = pending.dequeue()
//
//          transitions
//            .getOrElse(pendingState, Nil)
//            .foreach({
//              case (nextSymbol, nextState) => {
//                val nextString = pendingString + nextSymbol
//                if (livestates.contains(nextState)) {
//                  pending += nextString -> nextState
//                  if (finalStates.contains(nextState)) {
//                    pendingFinals += nextString
//                  }
//                }
//              }
//            })
//        }
//
//        if (pendingFinals.nonEmpty) {
//          Some(pendingFinals.dequeue())
//        } else {
//          None
//        }
//      }
//
//    def genLazyList: LazyList[String] = {
//      val str = nextStr
//      if (str.isDefined) {
//        str.get #:: genLazyList
//      } else {
//        LazyList.empty[String]
//      }
//    }
//    genLazyList
//  }
//
//  /**
//    * Given an FSM and a multiplier, return the multiplied FSM.
//    */
//  def times(multiplier: Int): Fsm = {
//    require(multiplier >= 0, "Can't multiply an FSM by " + multiplier)
//    //here we always work with "currentState -> iteration" pairs
//    def initial = Set(this.initialState -> 0)
//
//    def follow(crawlState: Set[(Int, Int)], symbol: Char): Option[Set[(Int, Int)]] = {
//
//      val next = crawlState
//        .filter { case (_, iteration) => iteration < multiplier }
//        .flatMap {
//          case (fsmState, iteration) =>
//            nextState(fsmState, symbol).map(
//              subState =>
//                if (finalStates.contains(subState)) {
//                  List(subState -> iteration, initialState -> (iteration + 1))
//                } else {
//                  List(subState -> iteration)
//                }
//            )
//        }
//        .flatten
//
//      Some(next).filter(_.nonEmpty)
//    }
//
//    //If the initial state is final then multiplying doesn't alter that
//    def isFinal(crawlState: Set[(Int, Int)]): Boolean = crawlState.exists {
//      case (fsmState, iteration) =>
//        (fsmState == initialState) &&
//          (finalStates.contains(initialState) || (iteration == multiplier))
//    }
//
//    Fsm.crawl(alphabet, initial, isFinal, follow).reduced
//  }
//
//  /**
//    * Return next state for this Fsm, if defined
//    */
//  def nextState(state: Int, symbol: Char): Option[Int] =
//    transitions.get(state).flatMap(_.get(symbol))
//
//  /**
//    * If the present FSM accepts X, returns an FSM accepting X* (i.e. 0 ormore Xes).
//    * This is NOT as simple as naively connecting the final states back to the initial state:
//    * see (b*ab)* for example.
//    */
//  def star: Fsm = {
//    def follow(subStates: Set[Int], symbol: Char): Option[Set[Int]] = {
//
//      val next = subStates
//        .flatMap(
//          subState =>
//            if (finalStates.contains(subState)) {
//              List(nextState(subState, symbol), nextState(initialState, symbol))
//            } else {
//              List(nextState(subState, symbol))
//            }
//        )
//        .flatten
//
//      Some(next).filter(_.nonEmpty)
//    }
//
//    def isFinal(subStates: Set[Int]): Boolean =
//      subStates.exists(subState => finalStates.contains(subState))
//
//    Fsm.crawl(alphabet, Set(initialState), isFinal, follow) | Fsm
//      .epsilonFsm(alphabet)
//  }
//
//  /**
//    * Consider the FSM as a set of strings and return the cardinality of thatset
//    * - Some[Int], or None if there are infinitely many
//    */
//  @SuppressWarnings(Array("org.wartremover.warts.Return"))
//  def cardinality: Option[Int] = {
//    val stateToCount = mutable.Map[Int, Option[Int]]()
//    //no tail recursion here
//    def getNumStrings(state: Int): Option[Int] =
//      if (isLive(state)) {
//        val stateCount = stateToCount.get(state)
//
//        Some(
//          stateCount
//            .getOrElse({
//              //None means "computing right now..."
//              stateToCount += state -> None
//              val n = (if (finalStates.contains(state)) {
//                         1
//                       } else {
//                         0
//                       }) + transitions
//                .get(state)
//                .map(transition => transition.values)
//                .getOrElse(Nil)
//                .map(
//                  nextState =>
//                    //yup, recursion here
//                    getNumStrings(nextState).getOrElse({
//                      //fail fast - we trapped into infinite recursion
//                      return None
//                    })
//                )
//                .sum
//
//              stateToCount += state -> Some(n)
//              Some(n)
//            })
//            .getOrElse({
//              //fail fast = we trapped into infinite recursion
//              return None
//            })
//        )
//      } else {
//        stateToCount += state -> Some(0)
//        Some(0)
//      }
//
//    getNumStrings(initialState)
//  }
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a subset of `that`... `this` recognises no strings which `that` doesn't.
//    */
//  def isSubset(that: Fsm): Boolean = (this - that).isEmpty
//
//  /**
//    * Same as IsSubset, and also checks that FSMs aren't equal.
//    */
//  def IsStrictSubset(that: Fsm): Boolean =
//    (this - that).isEmpty && (this != that)
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a superset of `that`...
//    */
//  def isSuperset(that: Fsm): Boolean = (that - this).isEmpty
//
//  /**
//    * Same as IsSuperset, and also checks that FSMs aren't equal.
//    */
//  def isStrictSuperset(that: Fsm): Boolean =
//    (that - this).isEmpty && (this != that)
//
//  /**
//    * Shortcut for the Cardinality method
//    */
//  def length: Option[Int] = cardinality
//
//  /**
//    * Symmetric difference. Returns an FSM which recognises only the strings
//    * recognised by `this` or `that` but not both.
//    */
//  def symmetricDifference(that: Fsm): Fsm = Fsm.symmetricDifference(this, that)
//
//  /**
//    * Difference. Returns an FSM which recognises only the strings
//    * recognised by the first FSM in the list, but none of the others.
//    */
//  def difference(that: Fsm): Fsm = Fsm.difference(this, that)
//
//  /**
//    * Treat the FSMs as sets of strings and return the intersection of those
//    * sets in the form of a new FSM.
//    */
//  def intersection(that: Fsm): Fsm = Fsm.intersection(this, that)
//
//  def union(that: Fsm): Fsm = Fsm.union(this, that)
//
//  /**
//    * Concatenate arbitrarily many finite state machines together.
//    */
//  def concatenate(that: Fsm): Fsm = Fsm.concatenate(this, that)
//
//  /**
//    * Concatenate arbitrarily many finite state machines together.
//    */
//  def concatenate(others: Fsm*): Fsm =
//    Fsm.concatenate(this :: others.toList: _*)
//
//  /**
//    * Treat `this` and `other` as sets of strings and see if they are disjoint
//    */
//  def isDisjoint(that: Fsm): Boolean = (this & that).isEmpty
//
//  /**
//    * An FSM is empty if it recognises no strings. An FSM may be arbitrarily
//    * complicated and have arbitrarily many final states while still recognising
//    * no strings because those final states may all be inaccessible from the
//    * initial state. Equally, an FSM may be non-empty despite having an empty
//    * alphabet if the initial state is final.
//    */
//  def isEmpty: Boolean = !isLive(initialState)
//
//  /**
//    * Two FSMs are considered equivalent if they recognise the same strings.
//    * Or, to put it another way, if their symmetric difference recognises no
//    * strings.
//    */
//  def equivalent(that: Fsm): Boolean = (this ^ that).isEmpty
//
//  /**
//    * Two FSMs are considered different if they have a non-empty symmetric difference.
//    */
//  def different(that: Fsm): Boolean = !(this ^ that).isEmpty
//
//  /**
//    * Returns a new FSM with empty and duplicate transitions removed
//    */
//  def reduced: Fsm = reversed.reversed
//
//  /**
//    * An alias for the 'Accepts' method
//    */
//  def contains(str: String): Boolean = accepts(str)
//
//  //region Operators
//
//  /**
//    * You can use `fsm1 == fsm2` to determine whether two FSMs recognise the same strings.
//    */
//  def ==(that: Fsm): Boolean = equivalent(that)
//
//  /**
//    * Two FSMs are considered different if they have a non-empty symmetric difference.
//    */
//  def !=(that: Fsm): Boolean = different(that)
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a subset
//    * of `that`... `this` recognises no strings which `that` doesn't.
//    */
//  def <=(that: Fsm): Boolean = isSubset(that)
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a strict subset of `that`.
//    */
//  def <(that: Fsm): Boolean = IsStrictSubset(that)
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a superset
//    * of `that`... `that` recognises no strings which `this` doesn't.
//    */
//  def >=(that: Fsm): Boolean = isSuperset(that)
//
//  /**
//    * Treat `this` and `that` as sets of strings and see if `this` is a strict superset of `that`.
//    */
//  def >(that: Fsm): Boolean = isStrictSuperset(that)
//
//  /**
//    * Symmetric difference. Returns an FSM which recognises only the strings recognised by `this` or `that` but not both.
//    */
//  def ^(that: Fsm): Fsm = symmetricDifference(that)
//
//  /**
//    * Treat the FSMs as sets of strings and return the intersection of those
//    * sets in the form of a new FSM.
//    */
//  def &(that: Fsm): Fsm = intersection(that)
//
//  /**
//    * Alternation.
//    * Return a finite state machine which accepts any sequence of symbols
//    * that is accepted by either self or other. Note that the set of strings
//    * recognised by the two FSMs undergoes a set union.
//    */
//  def |(that: Fsm): Fsm = union(that)
//
//  /**
//    * Concatenate 'this' and 'that' state machines.
//    */
//  def +(that: Fsm): Fsm = concatenate(that)
//
//  /**
//    * Difference. Returns an FSM which recognises only the strings
//    * recognised by the first FSM in the list, but none of the others.
//    */
//  def -(that: Fsm): Fsm = difference(that)
//
//  /**
//    * Given an FSM and a multiplier, return the multiplied FSM.
//    */
//  def *(multiplier: Int): Fsm = times(multiplier)
//
//  //endregion
//}
