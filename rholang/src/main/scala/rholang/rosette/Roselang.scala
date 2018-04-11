// -*- mode: Scala;-*-
// Filename:    Roselang.scala
// Authors:     luciusmeredith
// Creation:    Thu Feb  2 14:34:16 2017
// Copyright:   See site license
// Description:
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.lib.term._
import coop.rchain.rho2rose.StrTermCtorAbbrevs.StrTermCtxt
import coop.rchain.rholang.syntax.rholang._
import coop.rchain.rholang.syntax.rholang.Absyn._

import scala.language.implicitConversions
import scala.language.postfixOps

// B for Branch
object StrTermCtorAbbrevs {
  type ValOrVar = TagOrVar[String, String]
  type StrTermCtxt =
    TermCtxt[String, String, String] with RosetteSerialization[String, String, String]
  def Leaf(v: ValOrVar): StrTermCtxt                      = StrTermPtdCtxtLf(v)
  def B(v: String)(terms: StrTermCtxt*): StrTermCtxt      = StrTermPtdCtxtBr(v, terms.toList)
  def B(v: String, terms: List[StrTermCtxt]): StrTermCtxt = StrTermPtdCtxtBr(v, terms)
}

object VisitorTypes {
  // Arg Type
  // Set of bound names.
  type BoundSet = Set[String]
  type A        = Set[String]
  // Return Type
  type R = (StrTermCtorAbbrevs.StrTermCtxt, A)
}

object S2SImplicits {
  import StrTermCtorAbbrevs._
  implicit def asR(
      term: StrTermCtxt
  ): VisitorTypes.R = (term, Set[String]())
  implicit def asTerm(
      item: ValOrVar
  ): StrTermCtxt = StrTermPtdCtxtLf(item)
  implicit def asR(
      item: ValOrVar
  ): VisitorTypes.R = (StrTermPtdCtxtLf(item), Set[String]())
}

object CompilerExceptions {
  trait CompilerException
  trait SyntaxException
  trait SemanticsException

  case class UnexpectedContractType(
      c: Contr
  ) extends Exception(s"$c found in unexpected context")
      with CompilerException
      with SyntaxException
  case class NoComprehensionBindings(
      p: PInput
  ) extends Exception(s"$p has no bindings")
      with CompilerException
      with SyntaxException
  case class UnexpectedBindingType(
      b: Bind
  ) extends Exception(s"$b found in unexpected context")
      with CompilerException
      with SyntaxException
  case class UnexpectedBranchType(
      b: CBranch
  ) extends Exception(s"$b found in unexpected context")
      with CompilerException
      with SyntaxException
  case class UnexpectedPMBranchType(
      b: PMBranch
  ) extends Exception(s"$b found in unexpected context")
      with CompilerException
      with SyntaxException
  case class FailedQuotation(
      b: AnyRef
  ) extends Exception(s"$b found in unexpected context")
      with CompilerException
      with SyntaxException
  case class InternalCompilerError(
      b: AnyRef
  ) extends Exception(s"internal compiler error: $b")
      with CompilerException
  case class UnboundVariable(
      varName: String,
      line: Int,
      col: Int
  ) extends Exception(s"Unbound variable: $varName at $line:$col")
      with CompilerException
}

object ComprehensionOps {
  val _map    = "map"
  val _unit   = "unit"
  val _mult   = "mult"
  val _join   = "flatMap"
  val _filter = "filter"
}

object RosetteOps {
  val _abs      = "proc"
  val _defActor = "defActor"
  val _method   = "method"
  val _produce  = "produce"
  val _block    = "block"
  val _quote    = "Q"
  val _rx       = "RX"
  val _run      = "run"
  val _compile  = "compile"
  val _if       = "if"
  val _match    = "match-pattern" // TODO: Adjust based on Rosette implementation. This operation checks for equality in the "match" implementation.
  val _list     = "list" // TODO: Extract into "temporary operations" set
}

trait RholangASTToTerm extends AllVisitor[VisitorTypes.R, VisitorTypes.A] {
  import VisitorTypes._
  import StrTermCtorAbbrevs._
  import S2SImplicits._
  import CompilerExceptions._
  import RosetteOps._

  def TS(): StrTermCtxt = Var("t") // TODO: Replace with V( theTupleSpaceVar ) ?
  // TODO: Review cryptographic secureness and ensure Fresh and FreshSymbol are of uniform length
  // A FreshSymbol contains a quote in Rosette while a Fresh doesn't
  def Fresh() = {
    val prefix         = "Rholang"
    val uuidComponents = java.util.UUID.randomUUID.toString.split("-")
    prefix + uuidComponents(uuidComponents.length - 1)
  }
  def FreshSymbol(debugSymbol: String) =
    s"""(generateFresh "${debugSymbol}")"""

  /* TODO : rewrite this to be amenable to tail recursion elimination */
  def doQuote(expr: StrTermCtxt): StrTermCtxt =
    expr match {
      case leaf: StrTermPtdCtxtLf => {
        B(_quote)(leaf)
      }
      case StrTermPtdCtxtBr(op, subterms) => {
        val qterms = subterms.map((term) => doQuote(term))
        if (op == _list)
          B(_list, qterms)
        else
          B(_rx, Var(op) :: qterms)
      }
    }

  /* Contr */
  def visit(p: Contr, arg: A): R =
    p match {
      case dcontr: DContr => dcontr.proc_.accept(this, arg)
      case _              => throw new UnexpectedContractType(p)
    }
  // TODO: Handle case no arguments to contract
  override def visit(p: PContr, bound: A): R = {
    import scala.collection.JavaConverters._

    /*
     * [| contract <Name>( <formals> ) = { <body> } |]( t )
     * =
     * (let [[[products] (consume t [<Name>] [[<formals>]] #t) ]] // #t for persistent
     *     ((proc [[<formals>]] [| body |]) [products])
     * )
     *
     */
    def toListOfTuples(bindingsResults: List[(List[StrTermCtxt], A)]) =
      (bindingsResults map (x => x._1)) transpose match {
        case List(a, b, c) => (a, b, c)
        case List()        => (List(), List(), List())
      }

    def collectBindings(bindingsResults: List[(List[StrTermCtxt], A)]): A =
      (bound /: (bindingsResults map (x => x._2)))((acc: A, binding: A) => acc ++ binding)

    val ptrnTermList = p.listcpattern_.asScala.toList
    val bindingsResults: List[(List[StrTermCtxt], Set[String])] = ptrnTermList map {
      case ptrn: CPattern => {
        val ptrnResult = ptrn.accept(this, bound)
        // an explicit call to Leaf is necessary here, because rather than
        // apply an implicit, instead scala simply infers a useless type for
        // the list, and then fails to compile.
        val ptrnTerm       = ptrnResult._1
        val newlyBound     = ptrnResult._2
        val productFresh   = Leaf(Var(Fresh()))
        val quotedPtrnTerm = doQuote(ptrnTerm)._1
        (List(ptrnTerm, quotedPtrnTerm, productFresh), newlyBound)
      }
    }

    val pTerm: StrTermCtxt = p.proc_.accept(this, collectBindings(bindingsResults))._1
    val (formals, quotedFormals, productFreshes) = {
      val (formalsUnwrapped, quotedFormalsUnwrapped, productFreshesUnwrapped) = toListOfTuples(
        bindingsResults)
      (B(_list, formalsUnwrapped),
       B(_list, quotedFormalsUnwrapped),
       B(_list, productFreshesUnwrapped))
    }

    val consumeTerm     = B("consume")(TS, B(_list)(Tag(p.var_)), B(_list)(quotedFormals), Tag("#t"))
    val letBindingsTerm = B(_list)(B(_list)(productFreshes), consumeTerm)
    val bodyTerm        = B("")(B("proc")(B(_list)(formals), pTerm), productFreshes)
    B("")(
      B(_abs)(B(_list)(Tag("")),
              B(_run)(B(_compile)(B("let")(B(_list)(letBindingsTerm), bodyTerm)))))
  }

  override def visit(p: PNil, arg: A): R =
    Tag("#niv")
  override def visit(p: PValue, arg: A): R =
    p.value_.accept(this, arg)
  override def visit(p: PDrop, boundVars: A): R =
    /*
     *  Note that there are at least two different approaches to the
     *  lift/drop semantics. One is to compile the actuals supplied to
     *  lift to the target language and then let the target language
     *  supply the execution semantics for the drop. Dual to this one
     *  is to defer the compilation of the actuals to lift and then
     *  compile at drop time. This is more portable, though
     *  potentially less efficient. Another technique that comes into
     *  play is to calculate the hash of (each of) the actuals to lift
     *  and store the code at a name that is a function of the
     *  hash. Then send the hashes. Then at a drop the hash is used to
     *  recover the code. This technique can be composed with either
     *  of the other two techniques.
     */
    (p.chan_ match {
      case quote: CQuote => {
        quote.proc_.accept(this, boundVars)
      }
      case v: CVar => {
        if (boundVars(v.var_)) {
          B(_run)(B(_compile)(Var(v.var_)))
        } else {
          throw new UnboundVariable(v.var_, v.line_num, v.col_num)
        }
      }
    })

  override def visit(p: PLift, arg: A): R = {
    import scala.collection.JavaConverters._
    /*
     *  [| x!( P1, ..., PN ) |]( t )
     *  =
     *  ( produce t [| x ]( t ) `(,[| P1 |]( t )) ... `(,[| PN |]( t )) )
     */

    val actls =
      (List[StrTermCtxt]() /: p.listproc_.asScala.toList.reverse)(
        (acc, e) => e.accept(this, arg)._1 :: acc
      )

    val cTerm: StrTermCtxt = p.chan_.accept(this, arg)._1
    B(_produce, TS :: cTerm :: actls)
  }

  override def visit(p: PInput, bound: A): R = {
    import scala.collection.JavaConverters._

    def forToConsume(bindings: List[Bind]) = {
      def toListOfTuples(bindingsResults: List[(List[StrTermCtxt], A)]) =
        (bindingsResults map (x => x._1)) transpose match {
          case List(a, b, c, d) => (a, b, c, d)
          case List()           => (List(), List(), List(), List())
        }

      def collectBindings(bindingsResults: List[(List[StrTermCtxt], A)]): A =
        (bound /: (bindingsResults map (x => x._2)))((acc: A, binding: A) => acc ++ binding)

      val bindingsResults: List[(List[StrTermCtxt], BoundSet)] = bindings map {
        case inBind: InputBind => {
          val chanTerm: StrTermCtxt = inBind.chan_.accept(this, bound)._1
          val ptrnResult            = inBind.cpattern_.accept(this, bound)
          val ptrnTerm: StrTermCtxt = ptrnResult._1
          val newlyBound            = ptrnResult._2
          // explicit calls to Leaf are necessary here, because rather than
          // apply an implicit, instead scala simply infers a useless type for
          // the list, and then fails to compile.
          val productFresh   = Leaf(Var(Fresh()))
          val quotedPtrnTerm = doQuote(ptrnTerm)._1
          (List(chanTerm, ptrnTerm, quotedPtrnTerm, productFresh), newlyBound)
        }
        case _: CondInputBind =>
          throw new NotImplementedError("TODO: Handle condBind inside consume")
        case bind => throw new UnexpectedBindingType(bind)
      }
      val procTerm: StrTermCtxt                                   = p.proc_.accept(this, collectBindings(bindingsResults))._1
      val (chanTerms, ptrnTerms, quotedPtrnTerms, productFreshes) = toListOfTuples(bindingsResults)
      val consumeTerm                                             = B("consume")(TS, B(_list, chanTerms), B(_list, quotedPtrnTerms), Tag("#f")) // #f for persistent
      val wrappedPtrnTerms                                        = ptrnTerms map (x => B(_list)(x))
      val letBindingsTerm                                         = B(_list)(B(_list, productFreshes), consumeTerm)
      val bodyTerm =
        B("")(B("proc")(B(_list)(B(_list, wrappedPtrnTerms)), procTerm), B(_list, productFreshes))
      B("let")(B(_list)(letBindingsTerm), bodyTerm)
    }

    p.listbind_.asScala.toList match {
      case Nil => {
        throw new NoComprehensionBindings(p)
      }
      case bindings => {
        /*
         *  [[ for( ptrn <- chan; bindings )P ]]
         *  =
         *  (let [[[product1 product2 ... productN]
         *    (consume t [chanTerm1 chanTerm2 ... chanTermN] [ptrnTerm1 ptrnTerm2 ... ptrnTermN])]]
         *      ((proc [[ptrnTerm1 ptrnTerm2 ... ptrnTermN]] bodyTerm) [product1 product2 ... productN])
         *  )
         */
        forToConsume(bindings)
      }
    }
  }

  override def visit(p: PNew, arg: A): R = {
    import scala.collection.JavaConverters._
    val newVars            = p.listvar_.asScala.toList
    val newlyBound: A      = arg ++ newVars.toSet
    val pTerm: StrTermCtxt = p.proc_.accept(this, newlyBound)._1
    val newBindings = newVars.map({ (v) =>
      {
        val fresh = Var(FreshSymbol(v))
        B(_list)(Var(v), fresh)
      }
    })
    B("let")((B(_list, newBindings)), pTerm)
  }
  override def visit(p: PChoice, arg: A): R = {
    import scala.collection.JavaConverters._

    def cBranchToParPair(b: CBranch) =
      b match {
        case branch: Choice => {
          // babsurdity = 0
          val babsurdity = new PValue(new VQuant(new QInt(0)))

          // bmsg <- bchan
          val bmsgVStr = Fresh()
          val (bmsg, bchan) =
            (new CPtVar(new VarPtVar(bmsgVStr)), new CVar(Fresh()))
          val bbind = new InputBind(bmsg, bchan)

          // lmsg <- lchan
          val lmsgVStr = Fresh()
          val (lmsg, lchan) =
            (new CPtVar(new VarPtVar(lmsgVStr)), new CVar(Fresh()))
          val lbind = new InputBind(lmsg, lchan)

          val balertActls = new ListProc()
          balertActls.add(babsurdity)

          // case 1 => P_i | lchan!(0)
          val bvericase =
            new PatternMatch(new PPtVal(new VPtInt(1)),
                             new PPar(branch.proc_, new PLift(lchan, balertActls)))

          // case 0 => lchan!( 0 )
          val babsucase =
            new PatternMatch(
              new PPtVal(new VPtInt(0)),
              new PLift(lchan, balertActls)
            )

          val bcases = new ListPMBranch()
          bcases.add(bvericase)
          bcases.add(babsucase)

          val bsatActls = new ListProc()
          bsatActls.add(new PValue(new VQuant(new QInt(1))))

          val blocks = new ListBind()
          blocks.add(bbind)
          blocks.add(lbind)

          val bmatch = new PMatch(new PValue(new VQuant(new QVar(lmsgVStr))), bcases)

          val bpair =
            new PPar(
              // for( binding_i ){ bchan!( 1 ) }
              new PInput(branch.listbind_, new PLift(bchan, bsatActls)),
              // for( bmsg <- bchan; lmsg <- lchan ){
              //   match lmsg with
              //    case 1 => P_i | lchan!(0)
              //    case 0 => lchan!( 0 )
              // }
              new PInput(blocks, bmatch)
            )

          (bchan, bpair)
        }
        case _ => {
          throw new UnexpectedBranchType(b)
        }
      }

    p.listcbranch_.asScala.toList match {
      // select {} = Nil
      case Nil => {
        Tag("#niv")
      }
      // select { case bindings => P } = for( bindings )P
      case (branch: Choice) :: Nil => {
        visit(new PInput(branch.listbind_, branch.proc_), arg)
      }
      /*
       * select { case bindings1 => P1; ...; case bindingsN => PN }
       * =
       * new b1, ..., bN, lock in
       *   lock!( true )
       *   | for( bindings1 ){ b1!( true ) }
       *   | for( b <- b1; l <- lock ){
       *      match l with
       *       case true => P1 | lock!(false)
       *       case false => lock!( false )
       *     }
       *    ...
       *   | for( bindingsN ){ bN!( true ) }
       *   | for( b <- b1; l <- lock ){
       *      match l with
       *       case true => P1 | lock!(false)
       *       case false => lock!( false )
       *     }
       */
      case branches => {
        val (bvars, bpar :: rbpars) = branches.map(cBranchToParPair).unzip
        val bigbpar = (bpar /: rbpars)({ (acc, e) =>
          { new PPar(acc, e) }
        })
        val bnewVars = new ListVar()
        bvars.map({ (bvar) =>
          { bnewVars.add(bvar.var_) }
        })

        visit(new PNew(bnewVars, bigbpar), arg)
      }
    }
  }

  override def visit(p: PMatch, arg: A): R = {
    import scala.collection.JavaConverters._
    /*
     *  match <var> with
     *    case <bindings1> => P1;
     *    case <bindings2> => P2;
     *    ...
     *    case <bindingsN> => PN;
     *  =
     *  (if (match? <var> [[ bindings1 ]]) [[ P1 ]]
     *     (if (match? <var> [[ bindings2 ]]) ((proc [ [[ bindings2 ]] ] [[ P2 ]]) <var>) // Note a proc is generated only if there is a variable inside the <var> term
     *         ...
     *         (if (match? <var> [[bindingsN]]) [[ PN ]] #niv) // TODO: Handle nonexhaustive match by replacing #niv
     *     )
     *  )
     */

    def nonExhaustiveMatch: StrTermCtxt = Tag("#niv")

    val pTerm: StrTermCtxt = p.proc_.accept(this, arg)._1
    def patternMatchVisitAux: R = {
      val reverseListPMBranch = p.listpmbranch_.asScala.toList.reverse
      // We return the result of this fold
      (nonExhaustiveMatch /: reverseListPMBranch) { (acc, e) =>
        {
          e match {
            case pm: PatternMatch => {
              val patternResult: R          = pm.ppattern_.accept(this, arg)
              val pattern: StrTermCtxt      = patternResult._1
              val qPattern: StrTermCtxt     = doQuote(pattern)._1
              val patternBindings: BoundSet = arg ++ patternResult._2
              val continuation: StrTermCtxt = pm.proc_.accept(this, patternBindings)._1
              val remainder: StrTermCtxt    = acc
              if (isWild(pm.ppattern_)) {
                // Assumes VarPtWild comes at the end of a list of case statements
                continuation
              } else {
                def createProcForPatternBindings = {
                  val procTerm = B(_abs)(B(_list)(pattern), continuation)
                  B("")(procTerm, pTerm) // TODO: Potentially allow StrTermPtdCtxtBr without Namespace ?
                }

                val matchTerm = B(_match)(qPattern, pTerm)
                val matchTrueTerm = if (hasVariable(pm.ppattern_)) {
                  createProcForPatternBindings
                } else {
                  continuation
                }
                B(_if)(matchTerm, matchTrueTerm, remainder)
              }
            }
            case _ => throw new UnexpectedPMBranchType(e)
          }
        }
      }
    }

    patternMatchVisitAux
  }

  def isWild(p: PPattern): Boolean =
    p match {
      case pPtVar: PPtVar => {
        pPtVar.varpattern_ match {
          case _: VarPtWild => true
          case _            => false
        }
      }
      case _ => false
    }

  def hasVariable(p: PPattern): Boolean =
    // TODO: Fill in rest of cases
    p match {
      case _: PPtVar      => true
      case pPtVal: PPtVal => hasVariable(pPtVal.valpattern_)
    }

  def hasVariable(p: ValPattern): Boolean = {
    import scala.collection.JavaConverters._
    p match {
      case vPtTuple: VPtTuple => vPtTuple.listppattern_.asScala.toList.exists(hasVariable)
      case _                  => false
    }
  }

  override def visit(p: PConstr, arg: A): R = {
    import scala.collection.JavaConverters._
    /*
     *  [| <Name>( P1, ..., PN ) |]( t )
     *  =
     *  ( <Name> [| P1 |]( t ) ... [| PN |]( t ) )
     */
    val actls =
      (List[StrTermCtxt]() /: p.listproc_.asScala.toList.reverse)(
        (acc, e) => e.accept(this, arg)._1 :: acc
      )

    B(_produce, TS :: Leaf(Var(p.var_)) :: actls)
  }

  override def visit(p: PPar, arg: A): R = {
    // TODO: Collect all processes at the same level and place them in a single block.
    /*
     * [| P1 | P2 |]( t )
     * =
     * ( block [| P1 |]( t ) [| P2 |]( t ) )
     */
    val pTerm1: StrTermCtxt = p.proc_1.accept(this, arg)._1
    val pTerm2: StrTermCtxt = p.proc_2.accept(this, arg)._1
    B(_block)(pTerm1, pTerm2)
  }

  /* Chan */
  override def visit(p: CVar, boundVars: A): R =
    if (boundVars(p.var_)) {
      Var(p.var_)
    } else {
      throw new UnboundVariable(p.var_, p.line_num, p.col_num)
    }
  override def visit(p: CQuote, arg: A): R =
    // TODO: Handle quoting and unquoting
    p.proc_.accept(this, arg)
  /* Bind */
  // def visit( b : Bind, arg : A ) : R
  override def visit(p: InputBind, arg: A): R =
    throw new InternalCompilerError("Input bindings should not be visited directly.")
  /* CBranch */

  /* Value */
  override def visit(p: VQuant, arg: A): R =
    p.quantity_.accept(this, arg)
  /* Quantity */
  override def visit(p: QVar, boundVars: A): R =
    if (boundVars(p.var_)) {
      Var(p.var_)
    } else {
      throw new UnboundVariable(p.var_, p.line_num, p.col_num)
    }
  override def visit(p: QInt, arg: A): R =
    Tag(s"""${p.integer_}""")
  override def visit(p: QDouble, arg: A): R =
    Tag(s"""${p.double_}""")
  override def visit(p: QBool, arg: A): R =
    p.rhobool_.accept(this, arg)
  override def visit(p: QTrue, arg: A): R =
    Tag(s"""#t""")
  override def visit(p: QFalse, arg: A): R =
    Tag(s"""#f""")
  override def visit(p: QString, arg: A): R =
    Tag(s""""${p.string_}"""")
  override def visit(p: QMap, arg: A): R =
    Tag(s"""(new RblTable)""")
  override def visit(p: QDot, arg: A): R = {
    import scala.collection.JavaConverters._

    /* [[ quantity ]].method_name( [ [[ quantity_arg1 ]], [[ quantity_arg2 ]], ... ] )
     * =
     * (method_name quantity quantity_arg1 quantity_arg2)
     */
    val q: StrTermCtxt = p.quantity_.accept(this, arg)._1
    val qArgs =
      (List[StrTermCtxt]() /: p.listquantity_.asScala.toList.reverse)(
        (acc, e) => e.accept(this, arg)._1 :: acc
      )
    B("", Var(s"""${p.var_}""") :: q :: qArgs)
  }
  override def visit(p: QNeg, arg: A): R = {
    val q: StrTermCtxt = p.quantity_.accept(this, arg)._1
    B("-")(q)
  }
  override def visit(p: QMult, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("*")(q1, q2)
  }
  override def visit(p: QDiv, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("/")(q1, q2)
  }
  override def visit(p: QAdd, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("+")(q1, q2)
  }
  override def visit(p: QLt, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("<")(q1, q2)
  }
  override def visit(p: QLte, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("<=")(q1, q2)
  }
  override def visit(p: QGt, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B(">")(q1, q2)
  }
  override def visit(p: QGte, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B(">=")(q1, q2)
  }
  override def visit(p: QEq, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("=")(q1, q2)
  }
  override def visit(p: QNeq, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("!=")(q1, q2)
  }
  override def visit(p: QMinus, arg: A): R = {
    val q1: StrTermCtxt = p.quantity_1.accept(this, arg)._1
    val q2: StrTermCtxt = p.quantity_2.accept(this, arg)._1
    B("-")(q1, q2)
  }

  /* Entity */
  override def visit(p: EChar, arg: A): R =
    Tag(s"""'${p.char_}'""")
  override def visit(p: ETuple, arg: A): R = {
    import scala.collection.JavaConverters._
    val procTerms =
      (List[StrTermCtxt]() /: p.listproc_.asScala.toList.reverse)(
        (acc, e) => e.accept(this, arg)._1 :: acc
      )
    B(_list, procTerms)
  }

  /* Pattern */
  override def visit(p: VarPtVar, arg: A): R =
    // A variable in a pattern produces a new binding.
    (Var(p.var_), arg + p.var_)
  override def visit(p: VarPtWild, arg: A): R =
    (Var("**wildcard**"), arg)

  /* PPattern */
  override def visit(p: PPtVar, arg: A): R =
    p.varpattern_.accept(this, arg)
  override def visit(p: PPtVal, arg: A): R =
    p.valpattern_.accept(this, arg)

  /* CPattern */
  override def visit(p: CPtVar, arg: A): R =
    p.varpattern_.accept(this, arg)
  /* ValPattern */
  override def visit(p: VPtTuple, arg: A): R = {
    import scala.collection.JavaConverters._

    val tupleContents: (List[StrTermCtxt], BoundSet) =
      ((List[StrTermCtxt](), Set[String]()) /: p.listppattern_.asScala.toList.reverse)(
        (acc, e) => {
          val result = e.accept(this, arg)
          (result._1 :: acc._1, result._2 ++ acc._2)
        }
      )
    (B(_list, tupleContents._1), tupleContents._2)
  }
  override def visit(p: VPtStr, arg: A): R =
    Tag(s""""${p.string_}"""")
  override def visit(p: VPtTrue, arg: A): R =
    (Tag(s"""#t"""), arg)
  override def visit(p: VPtFalse, arg: A): R =
    (Tag(s"""#f"""), arg)
  override def visit(p: VPtInt, arg: A): R =
    (Tag(s"""${p.integer_}"""), arg)
  override def visit(p: VPtDbl, arg: A): R =
    (Tag(s"""${p.double_}"""), arg)
  override def visit(p: VPtNegInt, arg: A): R =
    (Tag(s"""-${p.integer_}"""), arg)
  override def visit(p: VPtNegDbl, arg: A): R =
    (Tag(s"""-${p.double_}"""), arg)

  override def visit(p: CPtQuote, arg: A): R      = ???
  override def visit(p: Choice, arg: A): R        = ???
  override def visit(p: DContr, arg: A): R        = ???
  override def visit(p: CValPtrn, arg: A): R      = ???
  override def visit(p: PatternMatch, arg: A): R  = ???
  override def visit(p: CondInputBind, arg: A): R = ???
}
