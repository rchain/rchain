package interpreter

sealed trait Channel extends Serializable

case class Quote(unquote: Proc) extends Channel {
  override def equals(o: Any): Boolean = {
    o match {
      case that @ Quote(n) => Proc.nameEquiv(this, that)
      case _               => super.equals(o)
    }
  }
  override def toString: String = "@(" + unquote + ")"
}

case class Var(id: String) extends Channel {
  override def toString: String = id
}

//* Term Constructors */
sealed trait Proc extends Serializable

//* 0 : 1 => P
case object Zero extends Proc {
  override def toString: String = "0"
}

//* ! : N x P => P */
final case class Output(x: Quote, q: Proc) extends Proc {
  override def toString: String = x.toString + "!(" + q.toString + ")"
}

//* for : N x N x P => P */
final case class Input(a: Action, k: Proc) extends Proc {
  override def toString: String = a.toString + "{ " + k.toString + " }"
}

//* | : P x P => P */
final case class Par(processes: List[Proc]) extends Proc {
  override def toString: String = {
    processes.map(p => p.toString).mkString(" | ")
  }
}

object Par {
  def apply(seqProc: Proc*): Par = Par(seqProc.toList)
}

//* * : N => P */
final case class Drop(x: Quote) extends Proc {
  override def toString: String = "*" + x.toString
}

//* Action(x,z) := x(z) := for(z <- x) */
final case class Action(nsubj: Quote, nobj: Quote) {
  override def toString: String =
    "for( " + nobj.toString + " <- " + nsubj.toString + " )"
}

object Proc {

  //* Smart Constructors */
  def zero: Zero.type = Zero

  def output(nsubj: Quote, cont: Proc): Output = Output(nsubj, cont)

  def input(nsubj: Quote, nobj: Quote, cont: Proc): Input =
    Input(Action(nsubj, nobj), cont)

  def par(proc1: Proc, proc2: Proc): Par = {
    (proc1, proc2) match {
      case (Par(xs0), Par(xs1)) => Par(xs0 ++ xs1)
      case (Par(xs), proc)      => Par(xs ++ List(proc))
      case (proc, Par(xs))      => Par(proc :: xs)
      case (p1, p2)             => Par(p1, p2)
    }
  }

 
  def parstar(xs: List[Proc]): Proc = {
    xs match {
      case Nil          => Zero //* Nil := Par() */
      case head :: tail => tail.foldLeft(head)((acc, proc) => par(acc, proc))
    }
  }

  def drop(n: Quote): Drop = Drop(n)

  def prefix(act: Action, cont: Proc): Input = {
    act match {
      case Action(Quote(proc1), Quote(proc2)) =>
        input(Quote(proc1), Quote(proc2), cont)
    }
  }

  //* Given a unique process, generate a unique name */
  def calcNextName: Proc => Quote = {
    case Zero           => Quote(Zero)
    case Drop(Quote(p)) => Quote(par(p, p))
    case Input(Action(Quote(psubj), Quote(pobj)), cont) =>
      Quote(parstar(List(psubj, pobj, cont)))
    case Output(Quote(psubj), cont) => Quote(par(psubj, cont))
    case Par(Nil)                   => Quote(Zero)
    case Par(head :: tail) =>
      Quote(tail.foldLeft(head) { (acc, proc) =>
        par(acc, proc)
      })
  }

  def syntacticSubstitution: Proc => Quote => Quote => Proc = {
    proc => nsource => ntarget =>
      proc match {

        case Zero => Zero

        case Drop(n) =>
          Drop(
            if (nameEquiv(n, ntarget)) nsource
            else n
          )

        case Input(Action(nsubj, nobj), cont) =>
          val obj: Quote = {
            if (nameEquiv(nobj, ntarget))
              calcNextName(Input(Action(nsubj, nobj), cont))
            else nobj
          }

          val subj: Quote = {
            if (nameEquiv(nsubj, ntarget)) nsource
            else nsubj
          }

          val cont_ : Proc = {
            syntacticSubstitution(
              if (nameEquiv(nobj, ntarget))
                syntacticSubstitution(cont)(obj)(nobj)
              else cont
            )(nsource)(ntarget)
          }

          Input(Action(subj, obj), cont_)

        case Output(nsubj, cont) =>
          val subj: Quote = {
            if (nameEquiv(nsubj, ntarget)) nsource
            else nsubj
          }

          Output(subj, syntacticSubstitution(cont)(nsource)(ntarget))

        case Par(xs) =>
          Par(xs.map(proc => syntacticSubstitution(proc)(nsource)(ntarget)))
      }
  }

  def alphaEquiv: Proc => Proc => Boolean = { proc1 => proc2 =>
    (proc1, proc2) match {
      case (Input(Action(nsubj1, nobj1), cont1),
            Input(Action(nsubj2, nobj2), cont2)) =>
        nameEquiv(nsubj1, nsubj2) && (cont1 == syntacticSubstitution(cont2)(
          nobj1)(nobj2))
      case (p1, p2) => p1 == p2
    }
  }

  /* 
    Differs from syntacticSubstitution because Drop(Quote(n))
    cancels to n.
  */
  def semanticSubstitution: Proc => Quote => Quote => Proc = {
    proc => nsource => ntarget =>
      proc match {

        case Zero => Zero

        case Drop(n) =>
          if (nameEquiv(n, ntarget)) {
            nsource match {
              case Quote(sProc) => sProc
              // case Var(_) => ???
            }
          } else Drop(n)

        case Input(Action(nsubj, nobj), cont) =>
          val obj: Quote = {
            if (nameEquiv(nobj, ntarget))
              calcNextName(Input(Action(nsubj, nobj), cont))
            else nobj
          }

          val subj: Quote = {
            if (nameEquiv(nsubj, ntarget)) nsource
            else nsubj
          }

          val cont_ : Proc = {
            syntacticSubstitution(
              if (nameEquiv(nobj, ntarget))
                semanticSubstitution(cont)(obj)(nobj)
              else cont
            )(nsource)(ntarget)
          }

          Input(Action(subj, obj), cont_)

        case Output(nsubj, cont) =>
          val subj: Quote = {
            if (nameEquiv(nsubj, ntarget)) nsource
            else nsubj
          }

          Output(subj, semanticSubstitution(cont)(nsource)(ntarget))

        case Par(xs) =>
          Par(xs.map(proc => semanticSubstitution(proc)(nsource)(ntarget)))
      }
  }

  def structurallyEquivalent(proc1: Proc, proc2: Proc): Boolean = {

    (proc1, proc2) match {

      case (Zero, Par(Nil)) => true

      case (Par(Nil), Zero) => true

      case (Zero, Par(head :: tail)) =>
        structurallyEquivalent(proc1, head) &&
          structurallyEquivalent(proc1, Par(tail))

      case (Par(head :: tail), Zero) =>
        structurallyEquivalent(Zero, Par(head :: tail))

      case (Input(Action(nsubj1, nobj1), cont1),
            Input(Action(nsubj2, nobj2), cont2)) =>
        nameEquiv(nsubj1, nsubj2) &&
          structurallyEquivalent(cont1,
                                 syntacticSubstitution(cont2)(nobj1)(nobj2))

      case (Par(head :: tail), Par(xs)) =>
        xs.partition(proc => structurallyEquivalent(head, proc)) match {
          case (Nil, tl) => false
          case (eqhd, eqtl) =>
            eqhd
              .foldLeft((false, List[Proc](), eqhd.tail)) { (rejects, proc) =>
                rejects match {
                  case (false, r, l) =>
                    if (structurallyEquivalent(parstar(r ++ l ++ eqtl),
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
        structurallyEquivalent(Par(head :: tail), Par(xs))

      case (_, Par(xs)) =>
        xs.partition(proc => structurallyEquivalent(proc1, proc)) match {
          case (Nil, procs)          => false
          case (List(eqproc), procs) => structurallyEquivalent(Zero, Par(procs))
          case (head :: tail, procs) => false
        }

      case (Par(xs), _) =>
        structurallyEquivalent(proc2, Par(xs))

      case (_, _) => proc1 == proc2
    }
  }

  def nameEquiv(n1: Quote, n2: Quote): Boolean = {
    (n1, n2) match {
      case (Quote(Drop(n11)), _)  => nameEquiv(n11, n2)
      case (_, Quote(Drop(n21)))  => nameEquiv(n1, n21)
      case (Quote(p1), Quote(p2)) => structurallyEquivalent(p1, p2)
    }
  }

  //* Collect the free names of a process into a set */
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

  def nameQuoteDepth(name: Quote): Int = {
    name match {
      case Quote(proc) => 1 + procQuoteDepth(proc)
    }
  }

  def procQuoteDepth(proc: Proc): Int = {
    proc match {
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
  }

  def matchIO(proc: Proc, procList: List[Proc]): (Option[Proc], List[Proc]) =
    procList match {
      case Nil => (None, procList)
      case hProc :: rProc =>
        proc match {
          case Zero    => (None, procList) //* 0 has no match */
          case Drop(n) => (None, procList) //* Drop has no match */
          case Par(prox) =>
            (None, procList) //* This could be extended pt-wise ... */

          case Input(Action(insubj, inobj), iproc) => //* Find a Lift( ... ) with matching subject */
            val ans: (List[Proc], List[Proc]) =
              procList.partition {
                case Output(onsubj, oproc) => nameEquiv(insubj, onsubj)
                case _                     => false
              }
            ans match {
              case (List(), _)          => (None, procList)
              case (mProc :: mrProc, procs) => (Some(mProc), mrProc ++ procs)
            }

          case Output(onsubj, oproc) => //* Find an Input( ... ) with matching subject */
            val ans: (List[Proc], List[Proc]) =
              procList.partition {
                case Input(Action(insubj, inobj), iproc) =>
                  nameEquiv(insubj, onsubj)
                case _ => false
              }
            ans match {
              case (List(), _)          => (None, procList)
              case (mProc :: mrProc, procs) => (Some(mProc), mrProc ++ procs)
            }
        }
    }

  def normalizeOnce(proc: Proc): (Boolean, Proc) =
    proc match {
      case Zero                => (false, Zero) //* stuck */
      case Input(act, cont)    => (false, Input(act, cont)) //* stuck */
      case Output(nsubj, cont) => (false, Output(nsubj, cont)) //* stuck */
      case Drop(nsubj)         => (false, Drop(nsubj)) //* stuck */

      case Par(List())     => (false, Par(List())) //* stuck */
      case Par(List(Zero)) => (false, Par(List(Zero))) //* stuck */
      case Par(List(Input(act, cont))) =>
        (false, Par(List(Input(act, cont))))  //* stuck */
      case Par(List(Output(nsubj, cont))) =>
        (false, Par(List(Output(nsubj, cont)))) //* stuck */
      case Par(List(Drop(nsubj))) =>
        (false, Par(List(Drop(nsubj))))  //* stuck */

      case Par(Par(hprox) :: rProc) =>
        normalizeOnce(Par(hprox ++ rProc)) //* mix */
      case Par(Zero :: rProc) =>
        normalizeOnce(Par(rProc)) //(* structural equivalence *)
      case Par(Drop(nsubj) :: rProc) => //* mix */
        normalizeOnce(Par(rProc ++ List(Drop(nsubj))))

      case Par(Input(Action(insubj, inobj), iproc) :: rProc) =>
        val mProc: (Option[Proc], List[Proc]) =
          matchIO(Input(Action(insubj, inobj), iproc), rProc)
        mProc match {
          case (None, nrProc) => //* mix */
            val mix = normalizeOnce(Par(rProc))
            mix match {
              case (b, Par(prox)) =>
                (b, Par(Input(Action(insubj, inobj), iproc) :: prox))
              case (b, redproc) =>
                sys.error(s"Invalid reduction result: $redproc") 
            }
          case (Some(Output(onsubj, oproc)), nrProc) => //* comm */
            val inputK = semanticSubstitution(iproc)(Quote(oproc))(inobj)
            (true, Par(nrProc ++ List(inputK)))
          case (Some(badMProc), rmproc) =>
            sys.error(s"Invalid reduction result: $badMProc") 
        }
      case Par(Output(onsubj, oproc) :: rProc) =>
        val mProc: (Option[Proc], List[Proc]) =
          matchIO(Output(onsubj, oproc), rProc)
        mProc match {
          case (None, nrProc) => //* mix */
            val mix = normalizeOnce(Par(rProc))
            mix match {
              case (b, Par(prox)) =>
                (b, Par(Output(onsubj, oproc) :: prox))
              case (b, redproc) =>
                sys.error(s"Invalid reduction result: $redproc")
            }
          case (Some(Input(Action(insubj, inobj), iproc)), nrProc) => //* comm */
            val inputK = semanticSubstitution(iproc)(Quote(oproc))(inobj)
            (true, Par(nrProc ++ List(inputK)))
          case (Some(badMProc), rmproc) =>
            sys.error(s"Invalid match result: $badMProc")
        }
    }

  def normalize(proc: Proc): Proc =
    normalizeOnce(proc) match {
      case (true, reduct) => normalize(reduct)
      case (false, fixpt) => fixpt
    }
}
