package ADT

sealed trait Channel extends Serializable

case class Quote(unquote: Proc) extends Channel {
  override def equals(o: Any): Boolean = {
    o match {
      case that @ Quote(n) => Proc.nameEquiv(this)(that)
      case _               => super.equals(o)
    }
  }
  override def toString: String = "@(" + unquote + ")"
}

case class Var(id: String) extends Channel {
  override def toString: String = id
}

sealed trait Proc extends Serializable

object Proc {

  def calculateNextName: Proc => Quote = {
    case Zero           => Quote(Zero)
    case Drop(Quote(p)) => Quote(par(p, p))
    case Input(Action(Quote(psubj), Quote(pobj)), cont) =>
      Quote(parstar(List(psubj, pobj, cont)))
    case Output(Quote(psubj), cont) => Quote(par(psubj, cont))
    case Par(Nil)                   => Quote(Zero)
    case Par(head :: tail) =>
      Quote(tail.foldLeft(head) { (acc, proc) =>
        Par(acc, proc)
      })
  }

  def substitute: Proc => Quote => Quote => Proc = {
    proc => nsource => ntarget =>
      proc match {

        case Zero => Zero

        case Drop(n) =>
          Drop(
            if (nameEquiv(n)(ntarget)) nsource
            else n
          )

        case Input(Action(nsubj, nobj), cont) =>
          val obj: Quote = {
            if (nameEquiv(nobj)(ntarget))
              calculateNextName(Input(Action(nsubj, nobj), cont))
            else nobj
          }

          val subj: Quote = {
            if (nameEquiv(nsubj)(ntarget)) nsource
            else nsubj
          }

          val cont_ : Proc = {
            substitute(
              if (nameEquiv(nobj)(ntarget)) substitute(cont)(obj)(nobj)
              else cont
            )(nsource)(ntarget)
          }

          Input(Action(subj, obj), cont_)

        case Output(nsubj, cont) =>
          val subj: Quote = {
            if (nameEquiv(nsubj)(ntarget)) nsource
            else nsubj
          }

          Output(subj, substitute(cont)(nsource)(ntarget))

        case Par(xs) =>
          Par(xs.map(proc => substitute(proc)(nsource)(ntarget)))
      }
  }

  /** A test for alpha-equivalence of expressions */
  def alphaEquivalent: Proc => Proc => Boolean = { proc1 => proc2 =>
    (proc1, proc2) match {
      case (Input(Action(nsubj1, nobj1), cont1),
      Input(Action(nsubj2, nobj2), cont2)) =>
        nameEquiv(nsubj1)(nsubj2) && (cont1 == substitute(cont2)(nobj1)(nobj2))
      case (p1, p2) => p1 == p2
    }
  }

  /*
    A test for structural equivalence of expressions.
    Notice, the bind function in RhoInterface is just a substitution
    that checks for name equivalence.
  */
  def structurallyEquivalent: Proc => Proc => Boolean = { proc1 => proc2 =>
    (proc1, proc2) match {

      case (Zero, Par(Nil)) => true

      case (Par(Nil), Zero) => true

      case (Zero, Par(head :: tail)) =>
        structurallyEquivalent(proc1)(head) &&
          structurallyEquivalent(proc1)(Par(tail))

      case (Par(head :: tail), Zero) =>
        structurallyEquivalent(Zero)(Par(head :: tail))

      case (Input(Action(nsubj1, nobj1), cont1),
      Input(Action(nsubj2, nobj2), cont2)) =>
        nameEquiv(nsubj1)(nsubj2) &&
          structurallyEquivalent(cont1)(substitute(cont2)(nobj1)(nobj2))

      case (Par(head :: tail), Par(xs)) =>
        xs.partition(proc => structurallyEquivalent(head)(proc)) match {
          case (Nil, tl) => false
          case (eqhd, eqtl) =>
            eqhd
              .foldLeft((false, List[Proc](), eqhd.tail)) { (rejects, proc) =>
                rejects match {
                  case (false, r, l) =>
                    if (structurallyEquivalent(parstar(r ++ l ++ eqtl))(
                      Par(tail))) (true, r, l)
                    else {
                      l match {
                        case Nil      => (false, r ++ List(proc), Nil)
                        case x :: xs1 => (false, r ++ List(proc), xs1)
                      }
                    }
                  case (true, r, l) => (true, r, l)
                }
              }
              ._1
        }

      case (Par(xs), Par(head :: tail)) =>
        structurallyEquivalent(Par(head :: tail))(Par(xs))

      case (_, Par(xs)) =>
        xs.partition(proc => structurallyEquivalent(proc1)(proc)) match {
          case (Nil, procs) => false
          case (List(eqproc), procs) =>
            structurallyEquivalent(Zero)(Par(procs))
          case (head :: tail, procs) => false
        }

      case (Par(xs), _) =>
        structurallyEquivalent(proc2)(Par(xs))

      case (_, _) => proc1 == proc2
    }
  }

  /** A test for name equivalence */
  def nameEquiv: Quote => Quote => Boolean = { n1 => n2 =>
    (n1, n2) match {
      case (Quote(Drop(n11)), _)  => nameEquiv(n11)(n2)
      case (_, Quote(Drop(n21)))  => nameEquiv(n1)(n21)
      case (Quote(p1), Quote(p2)) => structurallyEquivalent(p1)(p2)
    }
  }

  /** Collects the free names of an expressions */
  def free(proc: Proc): Set[Quote] = {
    proc match {
      case Zero                             => Set.empty[Quote]
      case Drop(n)                          => Set(n)
      case Input(Action(nsubj, nobj), cont) => free(cont) - nobj + nsubj
      case Output(nsubj, cont)              => free(cont) + nsubj
      case Par(xs) =>
        xs.foldLeft(Set.empty[Quote]) { (acc, proc) =>
          acc.union(free(proc))
        }
    }
  }

  def nameQuoteDepth: Quote => Int = {
    case Quote(proc) => 1 + procQuoteDepth(proc)
  }

  def procQuoteDepth: Proc => Int = {
    case Zero    => 0
    case Drop(n) => nameQuoteDepth(n)
    case Input(Action(nsubj, nobj), k) =>
      val qDSubj = nameQuoteDepth(nsubj)
      val qDCont = procQuoteDepth(k)
      math.max(qDSubj, qDCont)
    case Output(nsubj, k) =>
      val qDSubj = nameQuoteDepth(nsubj)
      val qDCont = procQuoteDepth(k)
      math.max(qDSubj, qDCont)
    case Par(xs) =>
      xs.foldLeft(0) { (qD, proc) =>
        val qDP = procQuoteDepth(proc)
        math.max(qD, qDP)
      }
  }



  /** 0 : 1 -> P */
  final case object Zero extends Proc {
    override def toString: String = "0"
  }

  def zero: Zero.type = Zero

  /** ! : N x P -> P */
  final case class Output(x: Quote, q: Proc) extends Proc {
    override def toString: String = x.toString + "!(" + q.toString + ")"
  }

  def lift(nsubj: Quote, cont: Proc): Output = Output(nsubj, cont)

  /** for : N x N x P -> P */
  final case class Input(a: Action, k: Proc) extends Proc {
    override def toString: String = a.toString + "{ " + k.toString + " }"
  }

  def input(nsubj: Quote, nobj: Quote, cont: Proc): Input =
    Input(Action(nsubj, nobj), cont)

  /** | : P x P -> P */
  final case class Par(processes: List[Proc]) extends Proc {
    override def toString: String = {
      processes.map(p => p.toString).mkString(" | ")
    }
  }

  /** a smart constructor for par given just two processes */
  def par(proc1: Proc, proc2: Proc): Par = {
    (proc1, proc2) match {
      case (Par(xs0), Par(xs1)) => Par(xs0 ++ xs1)
      case (Par(xs), proc)      => Par(xs ++ List(proc))
      case (proc, Par(xs))      => Par(proc :: xs)
      case (p1, p2)             => Par(List(p1, p2))
    }
  }

  /** a smart constructor for par given a list of processes */
  def parstar(procs: List[Proc]): Proc = {
    procs match {
      case Nil          => Zero
      case head :: tail => tail.foldLeft(head)((acc, proc) => par(acc, proc))
    }
  }

  object Par {
    def apply(seqProc: Proc*): Proc = parstar(seqProc.toList)
  }

  // * : N -> P
  final case class Drop(x: Quote) extends Proc {
    override def toString: String = "*" + x.toString
  }

  def drop(n: Quote): Drop = Drop(n)

  final case class Action(nsubj: Quote, nobj: Quote) {
    override def toString: String =
      "for( " + nobj.toString + " <- " + nsubj.toString + " )"
  }

  def prefix(act: Action, cont: Proc): Input = {
    act match {
      case Action(Quote(proc1), Quote(proc2)) =>
        input(Quote(proc1), Quote(proc2), cont)
    }
  }
}
