// -*- mode: Scala;-*- 
// Filename:    Roselang.scala 
// Authors:     luciusmeredith                                                    
// Creation:    Thu Feb  2 14:34:16 2017 
// Copyright:   See site license 
// Description: 
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.lib.term._
import coop.rchain.lib.zipper._
import coop.rchain.rho2rose.StrTermCtorAbbrevs.StrTermCtxt
import coop.rchain.syntax.rholang._
import coop.rchain.syntax.rholang.Absyn._

import scala.language.implicitConversions
import scala.language.postfixOps

// TODO: Check if we can move these to a specific file like "RoselangNavigation.scala"
// and see if we need change some of the new TermCtxtBranch[L,V,T] calls to more specific classes
trait StrTermNavigation extends TermNavigation[String,String,String]
trait StrTermMutation extends TermMutation [String,String,String]
trait StrTermZipperComposition extends TermZipperComposition[String,String,String]

// V for variable
// G for "ground" value
object StrTermCtorAbbrevs {
  type StrTermCtxt = TermCtxt[String,String,String] with Factual with RosetteSerialization[String,String,String]
  def V( v : String ) : StrTermCtxt = StrTermPtdCtxtLf( Var( v ) )
  def G( v : String ) : StrTermCtxt = StrTermPtdCtxtLf( Tag( v ) )
  def B( v : String )( terms : StrTermCtxt* ) = StrTermPtdCtxtBr( v, terms.toList )
}

object StrZipAbbrevs {
  type ValOrVar = TagOrVar[String,String]
  type LocVorV = Location[ValOrVar]
  def L( term : StrTermCtorAbbrevs.StrTermCtxt, ctxt : Context[ValOrVar] ) : LocVorV = Location( term, ctxt )   
  def T() : Context[ValOrVar] = Top()
}

object VisitorTypes {
  // Arg Type
  type A = Option[StrZipAbbrevs.LocVorV]
  // Return Type
  type R = Option[StrZipAbbrevs.LocVorV]    
}

object S2SImplicits {
  // A term may implicitly be converted to a location by placing it at the top
  // context.
  implicit def asLoc( 
    term : StrTermCtorAbbrevs.StrTermCtxt
  ) : StrZipAbbrevs.LocVorV = StrZipAbbrevs.L( term, StrZipAbbrevs.T() )
  implicit def asR( 
    loc : StrZipAbbrevs.LocVorV
  ) : VisitorTypes.R = Some( loc )
  implicit def asR(
    term : StrTermCtorAbbrevs.StrTermCtxt
  ) : VisitorTypes.R = asR( asLoc( term ) )
}

object CompilerExceptions {
  trait CompilerException
  trait SyntaxException
  trait SemanticsException

  case class UnexpectedContractType(
    c : Contr
  ) extends Exception( s"$c found in unexpected context" )
      with CompilerException with SyntaxException
  case class NoComprehensionBindings(
    p : PInput 
  ) extends Exception( s"$p has no bindings" )
      with CompilerException with SyntaxException
  case class UnexpectedBindingType(
    b : Bind
  ) extends Exception( s"$b found in unexpected context" )
      with CompilerException with SyntaxException
  case class UnexpectedBranchType(
    b : CBranch
  ) extends Exception( s"$b found in unexpected context" )
      with CompilerException with SyntaxException
  case class UnexpectedPMBranchType(
    b : PMBranch
  ) extends Exception( s"$b found in unexpected context" )
    with CompilerException with SyntaxException
  case class FailedQuotation(
    b : AnyRef
  ) extends Exception( s"$b found in unexpected context" )
      with CompilerException with SyntaxException
}

object ComprehensionOps {
  val _map = "map"
  val _unit = "unit"
  val _mult = "mult"
  val _join = "flatMap"
  val _filter = "filter"
}

object RosetteOps {
  val _abs = "proc"
  val _defActor = "defActor"
  val _method = "method"
  val _produce = "produce"
  val _block = "block"
  val _quote = "Q"
  val _rx = "RX"
  val _run = "run"
  val _compile = "compile"
  var _if = "if"
  var _match = "match?" // TODO: Adjust based on Rosette implementation. This operation checks for equality in the "match" implementation.
  var _list = "list" // TODO: Extract into "temporary operations" set
}

trait StrFoldCtxtVisitor
extends AllVisitor[VisitorTypes.R,VisitorTypes.A] {
  def zipr : StrTermNavigation with StrTermMutation with StrTermZipperComposition
}

trait RholangASTToTerm 
extends StrFoldCtxtVisitor {
  import VisitorTypes._
  import StrZipAbbrevs._
  import StrTermCtorAbbrevs._
  import S2SImplicits._
  import CompilerExceptions._
  import ComprehensionOps._
  import RosetteOps._

  def TS() = V("t") // TODO: Replace with V( theTupleSpaceVar ) ?
  // TODO: Review cryptographic secureness and ensure Fresh and FreshSymbol are of uniform length
  // A FreshSymbol contains a quote in Rosette while a Fresh doesn't
  def Fresh() = {
    val prefix = "Rholang"
    val uuidComponents = java.util.UUID.randomUUID.toString.split("-")
    prefix + uuidComponents(uuidComponents.length - 1)
  }
  def FreshSymbol( debugSymbol: String ) = {
    s"""(generateFresh "${debugSymbol}")"""
  }

  /* TODO : rewrite this to be amenable to tail recursion elimination */
  def doQuote( rexpr : R ) : R = {
    for( Location( expr : StrTermCtxt @unchecked, _ ) <- rexpr )
    yield {
      expr match {
        case leaf : StrTermPtdCtxtLf => {
          L( B( _quote )( leaf ), Top() )
        }
        case StrTermPtdCtxtBr( op, subterms ) => {
          val qterms = subterms.map( 
            { 
              ( term ) => { 
                (for( Location( qterm : StrTermCtxt @unchecked, _ ) <- doQuote( term ) )
                yield { qterm }).getOrElse( throw new FailedQuotation( term ) )
              }
            }
          )
          L( B( _rx )( (List( ( V( op ) ) ) ++ qterms):_* ), Top() )
        }
      }
    }
  }

  /* Contr */
  def visit( p : Contr, arg : A ) : R = {
    p match {
      case dcontr : DContr => dcontr.proc_.accept(this, arg)
      case _ => throw new UnexpectedContractType( p )
    }
  }
  // TODO: Handle case no arguments to contract
  override def visit( p : PContr, arg : A ) : R = {
    import scala.collection.JavaConverters._

    /*
     * [| contract <Name>( <formals> ) = { <body> } |]( t )
     * =
     * (let [[ [[binding1] [[arg1 arg2 ...]]] (consume t [<Name>] [**wildcard**] [[<formals>]] #t) ]] // #t for persistent
     *     ((proc [[<formals>]] [| body |]) [products])
     * )
     *
     */
    (for( Location( pTerm : StrTermCtxt @unchecked, _ ) <- p.proc_.accept(this, arg) )
    yield {

      def toListOfTuples(bindingsComponents: List[Option[List[StrTermCtxt]]]) = {
        bindingsComponents map {
          case Some(channelGroup) => channelGroup
        } transpose match {
          case List(a, b, c) => (a, b, c)
        }
      }

      val ptrnTermList = p.listcpattern_.asScala.toList
      val bindingsComponents = ptrnTermList map {
        case ptrn: CPattern => {
          for (
            Location(ptrnTerm: StrTermCtxt @unchecked, _) <- ptrn.accept(this, arg)
          ) yield {
            val productFresh = V(Fresh())
            val quotedPtrnTerm = (for (Location(q: StrTermCtxt @unchecked, _) <- doQuote(ptrnTerm)) yield {
              q
            }).getOrElse(throw new FailedQuotation(ptrnTerm))
            List(ptrnTerm, quotedPtrnTerm, productFresh)
          }
        }
      }

      val wildcard = V("**wildcard**")
      val unificationFresh = V(Fresh())
      val (formals, quotedFormals, productFreshes) =
        if (ptrnTermList.length == 1) {
          toListOfTuples(bindingsComponents) match {
            case (List(a), List(b), List(c)) => (a, b, c)
          }
        } else {
          val (formalsUnwrapped, quotedFormalsUnwrapped, productFreshesUnwrapped) = toListOfTuples(bindingsComponents)
          (B(_list)(formalsUnwrapped: _*), B(_list)(quotedFormalsUnwrapped: _*), B(_list)(productFreshesUnwrapped: _*))
        }

      val consumeTerm = B("consume")(TS, B(_list)( G(p.var_) ), B(_list)(wildcard), B(_list)(quotedFormals), G("#t"))
      val letBindingsTerm = B(_list)(B(_list)(B(_list)(unificationFresh), B(_list)(productFreshes)), consumeTerm)
      val bodyTerm = B("")(B("proc")(B(_list)(formals), pTerm), productFreshes)
      L(B("")(B(_abs)(B(_list)(G("")), B(_run)(B(_compile)(B("let")(B(_list)(letBindingsTerm), bodyTerm))))), T())
    })
  }

  /* Proc */
  override def visit(  p : PPrint, arg : A ) : R = {
    for(
      Location( pTerm : StrTermCtxt @unchecked, _ ) <- p.proc_.accept(this, arg )
    ) yield {
      val printTerm = B("print")(pTerm)
      val displayTerm = B("display")(G( "#\\\\n"))
      L( B( "seq" )( printTerm, displayTerm ), Top() )
    }
  }
  override def visit(  p : PNil, arg : A ) : R = {    
    Some( L( G( "#niv" ), T() ) )
  }
  override def visit(  p : PValue, arg : A ) : R = {
    p.value_.accept(this, arg )
  }
  override def visit(  p : PDrop, arg : A ) : R = {
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
    ( p.chan_ match {
      case quote : CQuote => {
        quote.proc_.accept(this, arg )
      }
      case v : CVar => {
        Some( L( B( _run )( B( _compile )( V( v.var_ ) ) ), Top() ) )
      }
    } )
  }

  override def visit(  p : PLift, arg : A ) : R = {
    import scala.collection.JavaConverters._
    /*
     *  [| x!( P1, ..., PN ) |]( t )
     *  =
     *  ( produce t [| x ]( t ) `(,[| P1 |]( t )) ... `(,[| PN |]( t )) )
     */

    val actls =
      ( List[StrTermCtxt]() /: p.listproc_.asScala.toList )(
        {
          ( acc, e ) => {
            e.accept(this, arg ) match {
              case Some( Location( pTerm : StrTermCtxt @unchecked, _ ) ) => {
                acc ++ List( pTerm )
              }
              case None => acc
            }
          }
        }
      )        

    for( Location( cTerm : StrTermCtxt @unchecked, _ ) <- p.chan_.accept(this, arg ) ) yield {
      L( B( _produce )( (List( TS ) ++ List(cTerm) ++ List(V("**wildcard**")) ++ actls):_* ), Top() )
    }
  }

  override def visit(  p : PInput, arg : A ) : R = {
    import scala.collection.JavaConverters._

    def forToConsume(bindings: List[Bind]) = {
      def toListOfTuples(bindingsComponents: List[Option[List[StrTermCtxt]]]) = {
        bindingsComponents map {
          case Some(channelGroup) => channelGroup
        } transpose match {
          case List(a, b, c, d, e, f) => (a, b, c, d, e, f)
        }
      }

      for (
        Location(procTerm: StrTermCtxt @unchecked, _) <- p.proc_.accept(this, arg)
      ) yield {
        val bindingsComponents = bindings map {
          case inBind: InputBind => {
            for (
              Location(chanTerm: StrTermCtxt @unchecked, _) <- inBind.chan_.accept(this, arg);
              Location(ptrnTerm: StrTermCtxt @unchecked, _) <- inBind.cpattern_.accept(this, arg)
            ) yield {
              val productFresh = V(Fresh())
              val unificationBindingFresh = V(Fresh())
              val wildcard = V("**wildcard**")
              val quotedPtrnTerm = inBind.cpattern_ match {
                case _ => (for (Location(q: StrTermCtxt @unchecked, _) <- doQuote(ptrnTerm)) yield {
                  q
                }).getOrElse(throw new FailedQuotation(ptrnTerm))
              }
              List(chanTerm, ptrnTerm, quotedPtrnTerm, productFresh, unificationBindingFresh, wildcard)
            }
          }
          case condBind: CondInputBind => throw new NotImplementedError("TODO: Handle condBind inside consume")
          case bind => throw new UnexpectedBindingType(bind)
        }
        val (chanTerms, ptrnTerms, quotedPtrnTerms, productFreshes, unificationFreshes, wildcards) = toListOfTuples(bindingsComponents)
        val consumeTerm = B("consume")(TS, B(_list)(chanTerms: _*), B(_list)(wildcards: _*), B(_list)(quotedPtrnTerms: _*), G("#f")) // #f for persistent
        val letBindingsTerm = B(_list)(B(_list)(B(_list)(unificationFreshes: _*), B(_list)(productFreshes: _*)), consumeTerm)
        val bodyTerm = B("")(B("proc")(B(_list)(B(_list)(ptrnTerms: _*)), procTerm), B(_list)(productFreshes: _*))
        L(B("let")(B(_list)(letBindingsTerm), bodyTerm), T())
      }
    }

    p.listbind_.asScala.toList match {
      case Nil => {
        throw new NoComprehensionBindings( p )
      }
      case binding :: Nil => {
        /*
         *  [[ for( ptrn <- chan )P ]]
         *  =
         *  (let [[[[unification_binding] [product]] (consume t [chanTerm] [**wildcard**] [ptrnTerm])]]
         *    ((proc [[ptrnTerm]] bodyTerm) [product]))
         */
        forToConsume(List(binding))
      }
      case bindings => {
        /*
         *  [[ for( ptrn <- chan; bindings )P ]]
         *  =
         *  (let [[[[unification_binding1 unification_binding2 ... unification_bindingN] [product1 product2 ... productN]]
         *    (consume t [chanTerm1 chanTerm2 ... chanTermN] [**wildcards** ... **wildcards**] [ptrnTerm1 ptrnTerm2 ... ptrnTermN])]]
         *      ((proc [[ptrnTerm1 ptrnTerm2 ... ptrnTermN]] bodyTerm) [product1 product2 ... productN])
         *  )
         */
        forToConsume(bindings)
      }
    }
  }

  override def visit(  p : PNew, arg : A ) : R = {
    import scala.collection.JavaConverters._
    val newVars = p.listvar_.asScala.toList
    (for( Location( pTerm : StrTermCtxt @unchecked, _ ) <- p.proc_.accept(this, arg ) )
    yield {
      val newBindings = newVars.map( { ( v ) => {
        val fresh = V(FreshSymbol(v))
        B(_list)(V( v ), fresh)
      } } )
      L( B( "let" )( ( List(B(_list)(newBindings:_*)) ++ List( pTerm )):_* ), Top() )
    })
  }
  override def visit(  p : PChoice, arg : A ) : R = {
    import scala.collection.JavaConverters._

    def cBranchToParPair( b : CBranch ) = {
      b match {
        case branch : Choice => {
          // bverity = 1, babsurdity = 0
          val ( bverity, babsurdity ) = 
            ( 
              new PValue( new VQuant( new QInt( 1 ) ) ), 
              new PValue( new VQuant( new QInt( 0 ) ) ) 
            )

          // bmsg <- bchan
          val bmsgVStr = Fresh()
          val ( bmsg, bchan ) = 
            ( new CPtVar( new VarPtVar( bmsgVStr ) ), new CVar( Fresh() ) )
          val bbind = new InputBind( bmsg, bchan )

          // lmsg <- lchan
          val lmsgVStr = Fresh()
          val ( lmsg, lchan ) = 
            ( new CPtVar( new VarPtVar( lmsgVStr ) ), new CVar( Fresh() ) )
          val lbind = new InputBind( lmsg, lchan )

          val balertActls = new ListProc()
          balertActls.add( babsurdity )

          // case 1 => P_i | lchan!(0)
          val bvericase =
            new PatternMatch( new PPtVal( new VPtInt( 1 ) ), new PPar(branch.proc_, new PLift( lchan, balertActls )))

          // case 0 => lchan!( 0 )
          val babsucase =
            new PatternMatch( 
              new PPtVal( new VPtInt( 0 ) ), 
              new PLift( lchan, balertActls ) 
            )
         
          val bcases = new ListPMBranch()
          bcases.add( bvericase )
          bcases.add( babsucase )

          val bsatActls = new ListProc()
          bsatActls.add( new PValue( new VQuant( new QInt( 1 ) ) ) )

          val blocks = new ListBind()
          blocks.add( bbind )
          blocks.add( lbind )

          val bmatch = new PMatch( new PValue( new VQuant( new QVar( lmsgVStr ) ) ), bcases )

          val bpair = 
            new PPar(
              // for( binding_i ){ bchan!( 1 ) }
              new PInput( branch.listbind_, new PLift( bchan, bsatActls ) ),
              // for( bmsg <- bchan; lmsg <- lchan ){
              //   match lmsg with
              //    case 1 => P_i | lchan!(0)
              //    case 0 => lchan!( 0 )
              // }          
              new PInput( blocks, bmatch )
            )

          ( bchan, bpair )
        }
        case _ => {
          throw new UnexpectedBranchType( b )
        }
      }
    }

    p.listcbranch_.asScala.toList match {
      // select {} = Nil
      case Nil => {
        Some( L( G( "#niv" ), T() ) )
      }
      // select { case bindings => P } = for( bindings )P
      case ( branch : Choice ) :: Nil => {
        visit( new PInput( branch.listbind_, branch.proc_ ), arg )
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
        val ( bvars, bpar :: rbpars ) = branches.map( cBranchToParPair ).unzip
        val bigbpar = ( bpar /: rbpars )( { ( acc, e ) => { new PPar( acc, e ) } } )
        val bnewVars = new ListVar()
        bvars.map( { ( bvar ) => { bnewVars.add( bvar.var_ ) } } )

        visit( new PNew( bnewVars, bigbpar ), arg )
      }
    }
  }

  override def visit(  p : PMatch, arg : A ) : R = {
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

    def nonExhaustiveMatch: R = L(G("#niv"), Top())

    def patternMatchVisitAux: R = {
      val result = for (Location(pTerm: StrTermCtxt @unchecked, _) <- p.proc_.accept(this, arg)) yield {
        val reverseListPMBranch = p.listpmbranch_.asScala.toList.reverse
        (nonExhaustiveMatch /: reverseListPMBranch) {
          (acc, e) => {
            e match {
              case pm: PatternMatch => {
                for (
                  Location(pattern: StrTermCtxt @unchecked, _) <- pm.ppattern_.accept(this, arg);
                  Location(continuation: StrTermCtxt @unchecked, _) <- pm.proc_.accept(this, arg);
                  Location(remainder: StrTermCtxt @unchecked, _) <- acc
                ) yield {
                  if (isWild(pm.ppattern_)) {
                    // Assumes VarPtWild comes at the end of a list of case statements
                    continuation
                  } else {
                    def createProcForPatternBindings = {
                      val procTerm = B(_abs)(B(_list)(pattern), continuation)
                      B("")(procTerm, pTerm) // TODO: Potentially allow StrTermPtdCtxtBr without Namespace ?
                    }

                    val matchTerm = B(_match)(pTerm, pattern)
                    val matchTrueTerm = if (hasVariable(pm.ppattern_)) {
                      createProcForPatternBindings
                    } else {
                      continuation
                    }
                    val ifTerm = B(_if)(matchTerm, matchTrueTerm, remainder)
                    L(ifTerm, Top())
                  }
                }
              }
              case _ => throw new UnexpectedPMBranchType(e)
            }
          }
        }
      }
      result match {
        case Some(r) => r
        case _ => throw new Exception()
      }
    }

    patternMatchVisitAux
  }

  def isWild(p: PPattern): Boolean = {
    p match {
      case pPtVar: PPtVar => {
        pPtVar.varpattern_ match {
          case wild : VarPtWild => true
          case _ => false
        }
      }
      case _ => false
    }
  }

  def hasVariable(p: PPattern): Boolean = {
    // TODO: Fill in rest of cases
    p match {
      case pPtVar : PPtVar => true
      case pPtNil : PPtNil => false
      case pPtVal : PPtVal => hasVariable( pPtVal.valpattern_ )
    }
  }

  def hasVariable(p: ValPattern) : Boolean = {
    import scala.collection.JavaConverters._
    p match {
      case vPtStruct : VPtStruct => vPtStruct.listppattern_.asScala.toList.exists(hasVariable)
      case vPtTuple: VPtTuple => vPtTuple.listppattern_.asScala.toList.exists(hasVariable)
      case _ => false
    }
  }

  override def visit(  p : PConstr, arg : A ) : R = {
    import scala.collection.JavaConverters._
    /*
     *  [| <Name>( P1, ..., PN ) |]( t )
     *  =
     *  ( <Name> [| P1 |]( t ) ... [| PN |]( t ) )
     */
    val actls =
      ( List[StrTermCtxt]() /: p.listproc_.asScala.toList )(
        {
          ( acc, e ) => {
            e.accept(this, arg ) match {
              case Some( Location( pTerm : StrTermCtxt @unchecked, _ ) ) => acc ++ List( pTerm )
              case None => acc
            }
          }
        }
      )        

    Some( L( B( p.var_ )( actls:_* ), Top() ) )
  }
  override def visit(  p : PPar, arg : A ) : R = {
    /*
     * [| P1 | P2 |]( t ) 
     * =
     * ( block [| P1 |]( t ) [| P2 |]( t ) )
     */
    for( 
      Location( pTerm1 : StrTermCtxt @unchecked, _ ) <- p.proc_1.accept(this, arg );
      Location( pTerm2 : StrTermCtxt @unchecked, _ ) <- p.proc_2.accept(this, arg )
    ) yield {
      L( B( _block )( pTerm1, pTerm2 ), Top() )
    }
  }

  /* Chan */
  override def visit(  p : CVar, arg : A ) : R = {
    Some( L( V( p.var_ ), T() ) )
  }
  override def visit(  p : CQuote, arg : A ) : R = {
    // TODO: Handle quoting and unquoting
    p.proc_.accept(this, arg )
  }
  /* Bind */
  // def visit( b : Bind, arg : A ) : R
  override def visit(  p : InputBind, arg : A ) : R = {
    arg match {
      // [[ P ]] is proc
      case Some( Location( proc : StrTermCtxt @unchecked, Top() ) ) => {
        for(
          // [[ chan ]] is chanTerm
          Location( chanTerm : StrTermCtxt @unchecked, _ ) <- p.chan_.accept(this, arg );
          // [[ ptrn ]] is ptrnTerm
          Location( ptrnTerm : StrTermCtxt @unchecked, _ ) <- p.cpattern_.accept(this, arg )
        ) yield {
          // ( map [[ chan ]] proc [[ ptrn ]] [[ P ]] )
          L( B( _map )( chanTerm, B( _abs )( B(_list)(ptrnTerm), proc ) ), T() )
        }
      }
      case _ => { // this is a little too optimistic or forgiving
        // should ensure that arg is a reasonable context to be really safe
        for(
          // [[ chan ]] is chanTerm
          Location( chanTerm : StrTermCtxt @unchecked, _ ) <- p.chan_.accept(this, arg );
          // [[ ptrn ]] is ptrnTerm
          Location( ptrnTerm : StrTermCtxt @unchecked, _ ) <- p.cpattern_.accept(this, arg );
          Location( rbindingsTerm : StrTermCtxt @unchecked, _ ) <- arg
        ) yield {
          // ( flatMap [[ chan ]] proc [[ ptrn ]] [[ for( bindings )P ]] )
          L( B( _join )( chanTerm, B( _abs )( B(_list)(ptrnTerm), rbindingsTerm ) ), T() )
        }
      }
    }
  }
  /* CBranch */

  /* Value */
  override def visit(  p : VQuant, arg : A ) : R = {
    p.quantity_.accept(this, arg )
  }
  /* Quantity */
  override def visit(  p : QVar, arg : A ) : R = {
    L(V(p.var_), Top())
  }
  override def visit(  p : QInt, arg : A ) : R = {
    L( G( s"""${p.integer_}"""), Top() )
  }
  override def visit(  p : QDouble, arg : A ) : R = {
    L( G( s"""${p.double_}"""), Top() )
  }
  override def visit( p : QBool, arg : A) : R = {
    p.rhobool_.accept(this, arg)
  }
  override def visit( p : QTrue, arg : A) : R = {
    L( G( s"""#t"""), Top() )
  }
  override def visit( p : QFalse, arg : A) : R = {
    L(G( s"""#f"""), Top())
  }
  override def visit(  p : QString, arg : A ) : R = {
    L( G( s""""${p.string_}"""" ), Top() )
  }
  override def visit( p : QMap, arg : A) : R = {
    L(G( s"""(new RblTable)"""), Top())
  }
  override def visit( p : QDot, arg : A) : R = {
    import scala.collection.JavaConverters._

    /* [[ quantity ]].method_name( [ [[ quantity_arg1 ]], [[ quantity_arg2 ]], ... ] )
     * =
     * (method_name quantity quantity_arg1 quantity_arg2)
     */
    for (Location( q : StrTermCtxt @unchecked, _ ) <- p.quantity_.accept(this, arg )) yield {
      val qArgs =
        ( List[StrTermCtxt]() /: p.listquantity_.asScala.toList )(
          {
            ( acc, e ) => {
              e.accept(this, arg ) match {
                case Some( Location( frml : StrTermCtxt @unchecked, _ ) ) => {
                  acc ++ List( frml )
                }
                case None => {
                  acc
                }
              }
            }
          }
        )
      L( B("")( (List(V(s"""${p.var_}""")) ++ List(q) ++ qArgs):_* ), Top() )
    }
  }
  override def visit( p : QNeg, arg : A) : R = {
    for( Location( q : StrTermCtxt @unchecked, _ ) <- p.quantity_.accept(this, arg ) ) yield {
      L( B("-")(q), Top() )
    }
  }
  override def visit( p : QMult, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("*")(q1,q2), Top() )
    }
  }
  override def visit( p : QDiv, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("/")(q1,q2), Top() )
    }
  }
  override def visit( p : QAdd, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("+")(q1,q2), Top() )
    }
  }
  override def visit( p : QLt, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("<")(q1,q2), Top() )
    }
  }
  override def visit( p : QLte, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("<=")(q1,q2), Top() )
    }
  }
  override def visit( p : QGt, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B(">")(q1,q2), Top() )
    }
  }
  override def visit( p : QGte, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B(">=")(q1,q2), Top() )
    }
  }
  override def visit( p : QEq, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("==")(q1,q2), Top() )
    }
  }
  override def visit( p : QNeq, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("!=")(q1,q2), Top() )
    }
  }


  override def visit( p : QMinus, arg : A) : R = {
    for(
      Location( q1 : StrTermCtxt @unchecked, _ ) <- p.quantity_1.accept(this, arg );
      Location( q2 : StrTermCtxt @unchecked, _ ) <- p.quantity_2.accept(this, arg )
    ) yield {
      L( B("-")(q1,q2), Top() )
    }
  }

  /* Entity */
  override def visit(  p : EChar, arg : A ) : R = {
    L( G( s"""'${p.char_}'"""), Top() )
  }
  override def visit(  p : ETuple, arg : A ) : R = {
    import scala.collection.JavaConverters._
    val procTerms =
      ( List[StrTermCtxt]() /: p.listproc_.asScala.toList )(
        {
          ( acc, e ) => {
            e.accept(this, arg ) match {
              case Some( Location( frml : StrTermCtxt @unchecked, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )
    L( B(_list)( procTerms:_* ), Top() )
  }

  /* Pattern */
  override def visit(p: VarPtVar, arg: A): R = {
    L(V(p.var_), Top())
  }
  override def visit(  p : VarPtWild, arg : A ) : R = {
    L(V("**wildcard**"), Top())
  }

  /* PPattern */
  override def visit(  p : PPtVar, arg : A ) : R = {
    p.varpattern_.accept(this, arg)
  }
  override def visit(  p : PPtVal, arg : A ) : R = {
    p.valpattern_.accept(this, arg)
  }

  /* CPattern */
  override def visit(p: CPtVar, arg: A): R = {
    p.varpattern_.accept(this, arg)
  }
  /* ValPattern */
  override def visit(  p : VPtStruct, arg : A ) : R = {
    import scala.collection.JavaConverters._
    
    val structContents =
      ( List[StrTermCtxt]() /: p.listppattern_.asScala.toList )(
        {
          ( acc, e ) => {
            e.accept(this, arg ) match {
              case Some( Location( frml : StrTermCtxt @unchecked, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )

    L( B(p.var_)( structContents:_* ), Top() )
  }
  override def visit(  p : VPtTuple, arg : A ) : R = {
    import scala.collection.JavaConverters._

    val tupleContents =
      ( List[StrTermCtxt]() /: p.listppattern_.asScala.toList )(
        {
          ( acc, e ) => {
            e.accept(this, arg ) match {
              case Some( Location( frml : StrTermCtxt @unchecked, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )

    L( B(_list)( tupleContents:_* ), Top() )
  }
  override def visit(  p : VPtTrue, arg: A ): R = {
    L( G( s"""#t"""), Top() )
  }
  override def visit(  p : VPtFalse, arg: A ): R = {
    L( G( s"""#f"""), Top() )
  }
  override def visit(  p : VPtInt, arg: A ): R = {
    L( G( s"""${p.integer_}"""), Top() )
  }
  override def visit(  p : VPtDbl, arg: A ): R = {
    L( G( s"""${p.double_}"""), Top() )
  }
  override def visit(  p : VPtNegInt, arg: A ): R = {
    L( G( s"""-${p.integer_}"""), Top() )
  }
  override def visit(  p : VPtNegDbl, arg: A ): R = {
    L( G( s"""-${p.double_}"""), Top() )
  }

  override def visit( p: PtBranch, arg: A ): R = ???
  override def visit( p: PtBind, arg: A ): R = ???
  override def visit( p: CPtQuote, arg: A ): R = ???
  override def visit( p: PPtPar, arg: A ): R = ???
  override def visit( p: PPtConstr, arg: A ): R = ???
  override def visit( p: PPtNew, arg: A ): R = ???
  override def visit( p: PPtMatch, arg: A ): R = ???
  override def visit( p: PPtInput, arg: A ): R = ???
  override def visit( p: PPtOutput, arg: A ): R = ???
  override def visit( p: PPtInject, arg: A ): R = ???
  override def visit( p: PPtDrop, arg: A ): R = ???
  override def visit( p: PPtNil, arg: A ): R = ???
  override def visit( p: Choice, arg: A ): R = ???
  override def visit( p: PInject, arg: A ): R = ???
  override def visit( p: DContr, arg: A ): R = ???
  override def visit( p: PFoldR, arg: A ): R = ???
  override def visit( p: PFoldL, arg: A ): R = ???
  override def visit( p: CValPtrn, arg: A ): R = ???
  override def visit( p: PatternMatch, arg: A ): R = ???
  override def visit( p: CondInputBind, arg: A ): R = ???
  override def visit( p: VPtStr, arg: A ): R = ???
}
