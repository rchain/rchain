// -*- mode: Scala;-*- 
// Filename:    Equivalence.scala 
// Authors:     Eitan Chatav                                                  
// Creation:    Thurs Dec 7 11:43:00 2017 
// Copyright:   See site license 
// Description: Spatial type checker; see Namespace Logic paper by LGM.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.syntax.rholang.Absyn._
import scalaz.{Bind => _, Value => _, _}
import scalaz.std.list._
import scalaz.std.option._
import scala.collection.JavaConverters._

object Equivalences{

  def nameEquivalent(env1: DeBruijn, n1: Chan, env2: DeBruijn, n2: Chan): Boolean = {
    (n1, n2) match {
      case (q1: CQuote, q2: CQuote) =>
        q1.proc_ match {
          case pdrop1: PDrop => nameEquivalent(pdrop1.chan_, n2)
          case _ => q2.proc_ match {
            case pdrop2: PDrop => nameEquivalent(n1, pdrop2.chan_)
            case _ => structurallyEquivalent(env1,q1.proc_,env2,q2.proc_)
          }
        }
      case (q1: CVar, q2: CVar) => env1.equivalent(q1.var_, env2, q2.var_)
      case _ => false
    }
  }

  def nameEquivalent(n1: Chan, n2: Chan): Boolean =
    nameEquivalent(DeBruijn(), n1, DeBruijn(), n2)

  def structurallyNil(p: Proc): Boolean = {
    p match {
      case _ : PNil => true
      case ppar : PPar =>
        structurallyNil(ppar.proc_1) && structurallyNil (ppar.proc_2)
      case _ => false
    }
  }

  def structurallyEquivalent(env1: DeBruijn, p1: Proc, env2: DeBruijn, p2: Proc): Boolean = {
    (p1,p2) match {
      case (_ : PNil, _) => structurallyNil(p2)
      case (_, _ : PNil) => structurallyNil(p1)
      case (v1: PValue, v2: PValue) => valueEquivalent(env1,v1.value_,env2,v2.value_)
      case (drop1: PDrop, drop2: PDrop) =>
        nameEquivalent(env1, drop1.chan_, env2, drop2.chan_)
      case (lift1: PLift, lift2: PLift) => {
        nameEquivalent(env1, lift1.chan_, env2, lift2.chan_) &&
          allStructurallyEquivalent(env1, lift1.listproc_, env2, lift2.listproc_)
      }
      case (input1: PInput, input2: PInput) =>
        allBindsEquivalent (
          env1, input1.listbind_.asScala.toList,
          env2, input2.listbind_.asScala.toList
        ) match {
          case None => false
          case Some((names1,names2)) =>
            allCPatternEquivalent(env1,names1,env2,names2) match {
              case None => false
              case Some((newEnv1,newEnv2)) =>
                structurallyEquivalent(newEnv1, input1.proc_, newEnv2, input2.proc_)
            }
        }
      case (_: PChoice, _: PChoice) => ???
      case (_: PMatch, _: PMatch) => ???
      case (_: PNew, _: PNew) => ???
      case (print1: PPrint, print2: PPrint) =>
        structurallyEquivalent(env1, print1.proc_, env2, print2.proc_)
      case (constr1: PConstr, constr2: PConstr) => {
        env1.equivalent(constr1.var_, env2, constr2.var_) &&
          allStructurallyEquivalent(env1, constr1.listproc_, env2, constr2.listproc_)
      }
      case (contr1: PContr, contr2: PContr) => {
        allCPatternEquivalent(
          env1, contr1.listcpattern_.asScala.toList,
          env2, contr2.listcpattern_.asScala.toList
          ) match {
            case None => false
            case Some((newenv1,newenv2)) =>
              env1.equivalent(contr1.var_, env2, contr2.var_) &&
                structurallyEquivalent(newenv1, contr1.proc_, newenv2, contr2.proc_)
          }
      }
      case (p1: PPar, p2: PPar) => parEquiv(parLeaves(p1),parLeaves(p2))
      case _ => false
    }
  }

  def bindEquivalent(env1: DeBruijn, b1: Bind, env2: DeBruijn, b2: Bind): Option[(CPattern,CPattern)] =
    (b1, b2) match {
      case (inpBind1: InputBind, inpBind2: InputBind) =>
        if (nameEquivalent(env1, inpBind1.chan_, env2, inpBind2.chan_)) {
          Some((inpBind1.cpattern_, inpBind2.cpattern_))
        } else None
      case (condInpBind1: CondInputBind, condInpBind2: CondInputBind) =>
        val check =
          nameEquivalent(env1, condInpBind1.chan_, env2, condInpBind2.chan_) &&
          structurallyEquivalent(env1, condInpBind1.proc_, env2, condInpBind2.proc_)
        if (check) {
          Some((condInpBind1.cpattern_, condInpBind2.cpattern_))
        } else None
      case _ => None 
    }

  def allBindsEquivalent(env1: DeBruijn, b1: List[Bind], env2: DeBruijn, b2: List[Bind]): Option[(List[CPattern],List[CPattern])] =
    b1 match {
      case Nil => b2 match {
        case Nil => Some((Nil,Nil))
        case _ => None
      }
      case head :: tail => b2.partition {
        bind => !(bindEquivalent(env1,bind,env2,head).isEmpty)
      } match {
        case (Nil,_) => None
        case (eqhd,eqtl) =>
          val init = (
            false,
            List[CPattern](),
            List[CPattern](),
            List[Bind](),
            eqhd.tail
          )
          val bindfold = eqhd.foldLeft(init) {
            (rejects, bnd) => rejects match {
              case (false, names1, names2, r, l) =>
                allBindsEquivalent(env1, r ++ l ++ eqtl, env2, tail) match {
                  case Some((name1,name2)) => (
                    true,
                    name1 ++ names1,
                    name2 ++ names2,
                    r, l
                  )
                  case None => l match {
                    case Nil => (
                      false, names1, names2,
                      r ++ List(bnd),
                      Nil
                    )
                    case x :: xs1 => (
                      false, names1, names2,
                      r ++ List(bnd),
                      xs1
                    )
                  }
                }
              case tuple => tuple
            }
          }
          bindfold match {
            case (false,_,_,_,_) => None
            case (true,names1,names2,_,_) => Some((names1,names2))
          }
        }
    }

  def alphaEquivalent(p1: Proc, p2: Proc): Boolean = ???

  def structurallyEquivalent(p1: Proc, p2: Proc): Boolean =
    structurallyEquivalent(DeBruijn(), p1, DeBruijn(), p2)
  
  def allStructurallyEquivalent(env1: DeBruijn, ps1: ListProc, env2: DeBruijn, ps2: ListProc): Boolean = {
    ps1.size() == ps2.size() &&
      (ps1.asScala.toList, ps2.asScala.toList).zipped.forall(
        (proc1,proc2) => structurallyEquivalent(env1, proc1, env2, proc2)
        )
  }

  def cpatternEquivalent(env1: DeBruijn, cp1: CPattern, env2: DeBruijn, cp2: CPattern): Option[(DeBruijn, DeBruijn)] = {
    (cp1, cp2) match {
      case (cpvar1: CPtVar, cpvar2: CPtVar) =>
        (cpvar1.varpattern_, cpvar2.varpattern_) match {
          case (_: VarPtWild, _: VarPtWild) => Some((env1,env2))
          case (v1: VarPtVar, v2: VarPtVar) => {
            Some((env1.newBindings(List(v1.var_)), env2.newBindings(List(v2.var_))))
          }
          case _ => None
        }
      case (cpval1: CValPtrn, cpval2: CValPtrn) => ???
      case (cpq1: CPtQuote, cpq2: CPtQuote) => ???
      case _ => None
    }
  }

  def allCPatternEquivalent(env1: DeBruijn, cps1: List[CPattern], env2: DeBruijn, cps2: List[CPattern]): Option[(DeBruijn, DeBruijn)] = {
    if (cps1.size != cps2.size) None else {
      val list = cps1.zip(cps2)
      def step(envs: (DeBruijn,DeBruijn), cps: (CPattern,CPattern)): Option[(DeBruijn,DeBruijn)] =
        cpatternEquivalent(envs._1, cps._1, envs._2, cps._2)
      Foldable[List].foldLeftM(list, (env1,env2)) (step _)
    }
  }
  
  def valueEquivalent(env1: DeBruijn, v1: Value, env2: DeBruijn, v2: Value): Boolean =
    (v1,v2) match {
      case (q1: VQuant, q2: VQuant) => q1 == q2
      case (c1: EChar, c2: EChar) => c1.char_ == c2.char_
      case (t1: ETuple, t2: ETuple) =>
        allStructurallyEquivalent(env1, t1.listproc_, env2, t2.listproc_)
      case _ => false
    }
  
  def parLeaves(proc: Proc): List[Proc] = proc match {
    case (_: PNil) => Nil
    case (p: PValue) => List(p)
    case (p: PDrop) => List(p)
    case (p: PLift) => List(p)
    case (p: PInput) => List(p)
    case (p: PChoice) => List(p)
    case (p: PMatch) => List(p)
    case (p: PNew) => List(p)
    case (p: PPrint) => List(p)
    case (p: PConstr) => List(p)
    case (p: PContr) => List(p)
    case (p: PPar) => parLeaves(p.proc_1) ++ parLeaves(p.proc_2)
  }

  def parEquiv(procs1: List[Proc], procs2: List[Proc]): Boolean = procs1 match {
    case Nil => procs2.forall(proc => structurallyNil(proc))
    case head :: tail =>
      procs2.partition(proc => structurallyEquivalent(head, proc)) match {
        case (Nil, tl) => false
        case (eqhd, eqtl) =>
          eqhd.foldLeft((false, List[Proc](), eqhd.tail)) {
            (rejects, proc) => rejects match {
              case (false, r, l) =>
                if (parEquiv(r ++ l ++ eqtl, tail)) (true, r, l)
                else {
                  l match {
                    case Nil => (false, r ++ List(proc), Nil)
                    case x :: xs1 => (false, r ++ List(proc), xs1)
                  }
                }
              case (true, r, l) => (true, r, l)
            }
          }._1
      }
  }
}

class DeBruijn(val environment: Map[String,Int], val next: Int){

  def this() = this(Map(), 1)

  def newBindings(bindings: List[String]): DeBruijn = {
    bindings.foldLeft(this) {
      (db: DeBruijn,str: String) =>
      DeBruijn(db.environment + (str -> db.next), db.next + 1)
    }
  }

  def get(key: String): Option[Int] = environment.get(key)

  def equivalent(key1: String, env: DeBruijn, key2: String) = {
    (get(key1), env.get(key2)) match {
      case (None,_) => false
      case (_,None) => false
      case (Some(ix1), Some(ix2)) => ix1 == ix2
      }
  }

}

object DeBruijn{

  def apply(): DeBruijn = new DeBruijn()

  def apply(environment: Map[String,Int], next: Int): DeBruijn = {
    new DeBruijn(environment, next)
  }

  def unapply(db: DeBruijn): Option[(Map[String,Int],Int)] = {
    Some((db.environment, db.next))
  }
}
