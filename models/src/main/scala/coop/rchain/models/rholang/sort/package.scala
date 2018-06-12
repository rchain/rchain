package coop.rchain.models.rholang

/**
  * Sorts the insides of the Par and ESet/EMap of the rholangADT
  *
  * A score tree is recursively built for each term and is used to sort the insides of Par/ESet/EMap.
  * For most terms, the current term type's absolute value based on the Score object is added as a Leaf
  * to the left most branch and the score tree built for the inside terms are added to the right.
  * The Score object is a container of constants that arbitrarily assigns absolute values to term types.
  * The sort order is total as every term type is assigned an unique value in the Score object.
  * For ground types, the appropriate integer representation is used as the base score tree.
  * For var types, the Debruijn level from the normalization is used.
  *
  * In order to sort an term, call [Type]SortMatcher.sortMatch(term)
  * and extract the .term  of the returned ScoredTerm.
  */
package object sort {

  sealed trait Tree[T]
  case class Leaf[T](item: T)                extends Tree[T]
  case class Node[T](children: Seq[Tree[T]]) extends Tree[T]

  case class ScoreAtom(value: Either[Int, String]) {
    def compare(that: ScoreAtom): Int =
      (this.value, that.value) match {
        case (Left(i1), Left(i2))   => i1.compare(i2)
        case (Left(_), Right(_))    => -1
        case (Right(_), Left(_))    => 1
        case (Right(s1), Right(s2)) => s1.compare(s2)
      }
  }

  object ScoreAtom {
    def apply(value: Int): ScoreAtom    = new ScoreAtom(Left(value))
    def apply(value: String): ScoreAtom = new ScoreAtom(Right(value))
  }

  object Leaf {
    def apply(item: Int)    = new Leaf(ScoreAtom(item))
    def apply(item: String) = new Leaf(ScoreAtom(item))
  }

  object Leaves {
    // Shortcut to be able to write Leaves(1,2,3) instead of Node(Seq(Leaf(1),Leaf(2),Leaf(3)))
    def apply(children: Int*): Node[ScoreAtom] = new Node(children.map(a => Leaf(a)))
  }

  object Node {
    // Shortcut to write Node(1, Leaf(1)) instead of Node(Seq(Leaf(ScoreAtom(1)), Leaf(ScoreAtom(1))))
    def apply(left: Int, right: Tree[ScoreAtom]*): Tree[ScoreAtom] =
      new Node(Seq(Leaf(left)) ++ right)
  }

  object ScoredTerm {
    implicit def ordering[T]: Ordering[ScoredTerm[T]] = new Ordering[ScoredTerm[T]] {
      override def compare(x: ScoredTerm[T], y: ScoredTerm[T]): Int = {
        def compareScore(s1: Tree[ScoreAtom], s2: Tree[ScoreAtom]): Int =
          (s1, s2) match {
            case (Leaf(a), Leaf(b)) => a.compare(b)
            case (Leaf(_), Node(_)) => -1
            case (Node(_), Leaf(_)) => 1
            case (Node(a), Node(b)) =>
              (a, b) match {
                case (Nil, Nil) => 0
                case (Nil, _)   => -1
                case (_, Nil)   => 1
                case (h1 +: t1, h2 +: t2) =>
                  compareScore(h1, h2) match {
                    case 0     => compareScore(Node(t1), Node(t2))
                    case other => other
                  }
              }
          }
        compareScore(x.score, y.score)
      }
    }
  }

  // Effectively a tuple that groups the term to its score tree.
  case class ScoredTerm[T](term: T, score: Tree[ScoreAtom])

  /**
    * Total order of all terms
    *
    * The general order is ground, vars, arithmetic, comparisons, logical, and then others
    */
  object Score {
    // For things that are truly optional
    final val ABSENT = 0

    // Ground types
    final val BOOL    = 1
    final val INT     = 2
    final val STRING  = 3
    final val URI     = 4
    final val PRIVATE = 5
    final val ELIST   = 6
    final val ETUPLE  = 7
    final val ESET    = 8
    final val EMAP    = 9

    // Vars
    final val BOUND_VAR = 50
    final val FREE_VAR  = 51
    final val WILDCARD  = 52

    // Expr
    final val EVAR     = 100
    final val ENEG     = 101
    final val EMULT    = 102
    final val EDIV     = 103
    final val EPLUS    = 104
    final val EMINUS   = 105
    final val ELT      = 106
    final val ELTE     = 107
    final val EGT      = 108
    final val EGTE     = 109
    final val EEQ      = 110
    final val ENEQ     = 111
    final val ENOT     = 112
    final val EAND     = 113
    final val EOR      = 114
    final val EMETHOD  = 115
    final val EBYTEARR = 116
    final val EEVAL    = 117

    // Other
    final val QUOTE    = 203
    final val CHAN_VAR = 204

    final val SEND              = 300
    final val RECEIVE           = 301
    final val NEW               = 303
    final val MATCH             = 304
    final val BUNDLE_EQUIV      = 305
    final val BUNDLE_READ       = 306
    final val BUNDLE_WRITE      = 307
    final val BUNDLE_READ_WRITE = 308

    final val CONNECTIVE_NOT    = 400
    final val CONNECTIVE_AND    = 401
    final val CONNECTIVE_OR     = 402
    final val CONNECTIVE_VARREF = 403

    final val PAR = 999
  }

}
