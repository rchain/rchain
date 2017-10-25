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

// TODO: Check if we can move these to a specific file like "RoselangNavigation.scala"
// and see if we need change some of the new TermCtxtBranch[L,V,T] calls to more specific classes
trait StrTermNavigation extends TermNavigation[String,Either[String,String],String]
trait StrTermMutation extends TermMutation [String,Either[String,String],String]
trait StrTermZipperComposition extends TermZipperComposition[String,Either[String,String],String]
trait StrTermSubstitution extends TermSubstitution[String,Either[String,String],String]

// V for language variable
// K for "context" variable
// G for "ground" value
object StrTermCtorAbbrevs {
  type StrTermCtxt = TermCtxt[String,Either[String,String],String] with Factual with RosetteSerialization[String,Either[String,String],String]
  def V( v : String ) : StrTermCtxt = StrTermPtdCtxtLf( Right( Left( v ) ) )
  def K( v : String ) : StrTermCtxt = StrTermPtdCtxtLf( Right( Right( v ) ) )
  def G( v : String ) : StrTermCtxt = StrTermPtdCtxtLf( Left( v ) )
  def B( v : String )( terms : StrTermCtxt* ) = StrTermPtdCtxtBr( v, terms.toList )
}

object StrZipAbbrevs {
  type ValOrVar = Either[String,Either[String,String]]
  type LocVorV = Location[ValOrVar]
  def L( term : StrTermCtorAbbrevs.StrTermCtxt, ctxt : Context[ValOrVar] ) : LocVorV = Location( term, ctxt )   
  def HV( cv : String ) : LocVorV = 
    Location( StrTermCtorAbbrevs.K( cv ), Top[ValOrVar]() )
  def T() : Context[ValOrVar] = Top()
}

object VisitorTypes {
  type A = Option[StrZipAbbrevs.LocVorV]
  type R = Option[StrZipAbbrevs.LocVorV]    
}

object S2SImplicits {
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
  case class UnexpectedCombination(
    xLoc : StrZipAbbrevs.LocVorV,
    yLoc : StrZipAbbrevs.LocVorV
  ) extends Exception( s"attempting to combine: $xLoc with $yLoc" )
      with CompilerException
}

object ComprehensionOps {
  val _map = "map"
  val _unit = "unit"
  val _mult = "mult"
  val _join = "flatMap"
  val _filter = "filter"
}

object RosetteOps {
  val _abs = "PX"
  val _defActor = "defActor"
  val _method = "method"
  // TODO: Extract quote marks
  val _produce = "'produce"
  val _consume = "'consume"
  val _block = "BX"
  val _quote = "Q"
  val _rx = "RX"
  val _seq = "SqX"
  var _let = "new LetExpr"
  val _run = "'run"
  val _compile = "'compile"
  var _if = "if"
  var _match = "match?" // TODO: Adjust based on Rosette implementation. This operation checks for equality in the "match" implementation.
  var _list = "TX" // TODO: Extract into "temporary operations" set
  val _wildcard = "'**wildcard**"
}

trait StrFoldCtxtVisitor
extends FoldVisitor[VisitorTypes.R,VisitorTypes.A] {
  def zipr : StrTermNavigation with StrTermMutation with StrTermZipperComposition
  def theCtxtVar : String

  def wrap( context : VisitorTypes.R ) : VisitorTypes.R = context
  def leaf( context : VisitorTypes.R ) : VisitorTypes.R = wrap( context )    

  override def combine(
    y : VisitorTypes.R,
    x : VisitorTypes.R,
    context : VisitorTypes.R
  ) : VisitorTypes.R = {
    /*
     println(
      (
	"/* ------------------------------------------------------- */\n"
	+ "/* method: " + "combine" + " */\n"
	+ "/* x: " + x + " */\n"
	+ "/* y: " + y + " */\n"
	+ "/* context: " + context + " */\n"
	+ "/* ------------------------------------------------------- */\n"
      )
    )
    */
    
    val rslt =
      for( 
	xLoc@Location( xTerm : StrTermCtorAbbrevs.StrTermCtxt, xCtxt ) <- x;
	yLoc@Location( yTerm : StrTermCtorAbbrevs.StrTermCtxt, yCtxt ) <- y
      ) yield {
	/*
	 println(
	  (
	    "/* ------------------------------------------------------- */\n"
	    + "/* method: " + "combine" + " continued" + " */\n"
	    + "/* xLoc: " + xLoc + " */\n"
	    + "/* yLoc: " + yLoc + " */\n"
	    + "/* ------------------------------------------------------- */\n"
	  )
	)
	*/
	yLoc match {
	  case Location( StrTermPtdCtxtLf( Right( Left( v ) ) ), Top( ) ) => {
            throw new CompilerExceptions.UnexpectedCombination( xLoc, yLoc )
          }
          case Location( StrTermPtdCtxtLf( Right( Right( v ) ) ), Top( ) ) => xLoc
	  case Location( _, Top( ) ) => {
	    xCtxt match {
	      case Top() => {
		val loc = zipr.up( zipr.insertDown( yLoc, xTerm ) )
		/*
		 * println(
		  (
		    "/* ------------------------------------------------------- */\n"
		    + "/* method: " + "combine" + " continued" + " */\n"
		    + "/* loc: " + loc + " */\n"
		    + "/* ------------------------------------------------------- */\n"
		  )
		)
		*/
		    
		loc
	      }
	      case _ => {
		val loc = zipr.update( yLoc, xTerm )
		/*
		 println(
		  (
		    "/* ------------------------------------------------------- */\n"
		    + "/* method: " + "combine" + " continued" + " */\n"
		    + "/* loc: " + loc + " */\n"
		    + "/* ------------------------------------------------------- */\n"
		  )
		)
		*/
		
		loc
	      }
	    }	    
	  }
	  case _ => {
	    xLoc match {
              case Location( StrTermPtdCtxtLf( Right( Left( v ) ) ), Top() ) => {
                throw new CompilerExceptions.UnexpectedCombination( xLoc, yLoc )
              }
	      case Location( StrTermPtdCtxtLf( Right( Right( v ) ) ), Top() ) => {
		val loc = zipr.update( yLoc, xTerm )
		/*
		 println(
		  (
		    "/* ------------------------------------------------------- */\n"
		    + "/* method: " + "combine" + " continued" + " */\n"
		    + "/* loc: " + loc + " */\n"
		    + "/* ------------------------------------------------------- */\n"
		  )
		)
		*/
		  		
		if ( ( v ).equals( theCtxtVar ) ) {		
		  loc
		}
		else {
		  zipr.up( loc )
		}		
	      }
	      case Location( _, Top() ) => {
		val loc = zipr.up( zipr.update( yLoc, xTerm ) )
		/*
		 println(
		  (
		    "/* ------------------------------------------------------- */\n"
		    + "/* method: " + "combine" + " continued" + " */\n"
		    + "/* loc: " + loc + " */\n"
		    + "/* ------------------------------------------------------- */\n"
		  )
		)
		*/
		
		loc
	      }
	      case Location(_, LabeledTreeContext(lbl: String, left, nrCtxt, right)) => {
		Location[StrZipAbbrevs.ValOrVar](
		  xTerm,
		  zipr.compose( yCtxt, xCtxt )
		)
	      }
	    }
	  }
	}
			
      }

    /*
     println(
      (
	"/* ------------------------------------------------------- */\n"
	+ "/* method: " + "combine" + " continued" + " */\n"
	+ "/* rslt: " + rslt + " */\n"
	+ "/* ------------------------------------------------------- */\n"
      )
    )
    */

    rslt
  }
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

  def theTupleSpaceVar : String
  def TS() = V("'t") // TODO: Replace with V( theTupleSpaceVar ) ?
  def CH() = V( theCtxtVar )
  def H() = HV( theCtxtVar )
  def Here() = Some( HV( theCtxtVar ) )
  def Fresh() = {
    val prefix = "rholang"
    val uuidComponents = java.util.UUID.randomUUID.toString.split( "-" )
    prefix + uuidComponents( uuidComponents.length - 1 )
  }

  def isTopLevel( r : R ) : Boolean = {
    r match {
      case Some( Location( StrTermPtdCtxtLf( Left( v ) ), Top() ) ) if v.equals( theCtxtVar ) => {
        true
      }
      case _ => false
    }
  }

  /* TODO : rewrite this to be amenable to tail recursion elimination */
  def doQuote( rexpr : R ) : R = {
    for( Location( expr : StrTermCtxt, _ ) <- rexpr )
    yield {
      expr match {
        case leaf : StrTermPtdCtxtLf => {
          L( B( _quote )( leaf ), Top() )
        }
        case StrTermPtdCtxtBr( op, subterms ) => {
          val qterms = subterms.map( 
            { 
              ( term ) => { 
                (for( Location( qterm : StrTermCtxt, _ ) <- doQuote( term ) )
                yield { qterm }).getOrElse( throw new FailedQuotation( term ) )
              }
            }
          )
          L( B( _rx )( (List( B( _quote )( V( op ) ) ) ++ qterms):_* ), Top() )
        }
      }
    }
  }

  def combine( ctxt1 : A, ctxt2 : R ) : R =
    combine( ctxt1, ctxt2, Here() )

  /* The signature of the basic compilation action is 
   * 
   *      def visit[T]( p : T, arg : A ) : R
   * 
   * Where T is the type of expression being compiled and arg is the
   * context into which the result of the compilation will be placed.
   * For example, the compilation of the Nil process is the rosette
   * expression #niv (no intrinsic value). This expression will be
   * placed into the context represented by arg.
   * 
   * The compilation process is made completely regular
   * by certain embeddings. This means we have a single data type for
   * both the context, A and the result R. This regularity ends up
   * being forced by how the visitor pattern works together with
   * certain coherence requirements on all the bodies of the visit
   * method definitions. The embeddings are as follows: 
   * 
   *   1. every expression e lifts to a tree, * t = unit( e ),
   *      which is just e regarded as a tree; 
   *   2. every tree t lifts to a location, l = L( t, T() );
   *   3. every context c can be lifted to a location l = L( V( "*H*" ), c )
   *   4. the composition of context with tree can be uniquely lifted
   *      to a composition of locations of the form
   *      l1 = L( V( "*H*" ), c )
   *      l2 = L( t, T() )
   *      (See the combine method above.)
   * 
   *  So every visit body will be of the form:
   * 
   *     combine( 
   *       arg,
   *       ( context( p ) /: p.parts )( 
   *          { 
   *             ( acc, e ) => {
   *                combine( acc, visit( e, L( V( "*H*" ), T() ) ) ) 
   *             }
   *          }
   *       )
   *     )
   * 
   *  where p.parts stands in for accessing the components of the
   *  expression p. 
   * 
   *  This folds over the parts accumulating the results of compiling
   *  the sub-expressions of p, and then placing them into the right
   *  piece of the compilation p, and then finally places the result
   *  of the fold into the context supplied by arg. Of course, p is
   *  not generally a collection of its parts and so the access of
   *  of the parts of p will be specific to the structure of the
   *  expression, p. Likewise, the combination of the results of the
   *  compilation of the components of p will be more specific than a
   *  fold. However, this gives the general intuition behind how this
   *  algorithm works. Furthermore, it works generally for any CFL.
   * 
   *  This method favors regularity and ease of reasoning over
   *  efficiency. However, it is vastly more efficient than the
   *  parser combinators method provided out of the box by Scala as
   *  testing on a parser for prolog revealed in production.
   */

  /* Contr */
  def visit( p : Contr, arg : A ) : R = {
    p match {
      case dcontr : DContr => visitDispatch( dcontr.proc_, arg )
      case _ => throw new UnexpectedContractType( p )
    }
  }
  // TODO: Handle case no arguments to contract
  override def visit( p : PContr, arg : A ) : R = {
    import scala.collection.JavaConverters._

    /*
     * [| contract <Name>( <formals> ) = { <body> } |]( t )
     *
     * =
     *
     * (proc [] (run (compile
     *  (let [[ [[binding1] [[arg1 arg2 ...]]] (consume t [<Name>] [**wildcard**] [[<formals>]] #t) ]] // #t for persistent
     *    ((proc [[<formals>]] [| body |]) [products])))))
     *
     */
    combine(
      arg,
      (for( Location( pTerm : StrTermCtxt, _ ) <- visitDispatch( p.proc_, Here() ) )
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
              Location(ptrnTerm: StrTermCtxt, _) <- visitDispatch(ptrn, Here())
            ) yield {
              var productFresh = V(s"""'${Fresh()}""")
              val quotedPtrnTerm = (for (Location(q: StrTermCtxt, _) <- doQuote(ptrnTerm)) yield {
                q
              }).getOrElse(throw new FailedQuotation(ptrnTerm))
              List(ptrnTerm, quotedPtrnTerm, productFresh)
            }
          }
        }


        val wildcard = V(_wildcard)
        val unificationFresh = V(s"""'${Fresh()}""")
        val (formals, quotedFormals, productFreshes) =
          if (ptrnTermList.length == 1) {
            toListOfTuples(bindingsComponents) match {
              case (List(a), List(b), List(c)) => (a, b, c)
            }
          } else {
            val (formalsUnwrapped, quotedFormalsUnwrapped, productFreshesUnwrapped) = toListOfTuples(bindingsComponents)
            (B(_list)(formalsUnwrapped: _*), B(_list)(quotedFormalsUnwrapped: _*), B(_list)(productFreshesUnwrapped: _*))
          }

        val consumeTerm = B(_rx)(G(_consume), TS, B(_list)( G(s"""'${p.var_}""") ), B(_list)(wildcard), B(_list)(quotedFormals), G("'#t")) // TODO: Switch to true when bindings can be injected
        val letBindingsTerm = B(_list)(B(_list)(B(_list)(unificationFresh), B(_list)(productFreshes)), consumeTerm)
        val bodyTerm = B(_rx)(B(_abs)(B(_list)(formals), pTerm), productFreshes)
        L(B(_rx)(B(_abs)(B(_list)(G("")), B(_rx)(G(_run), B(_rx)(G(_compile), B(_let)(B(_list)(letBindingsTerm), bodyTerm))))), T())
      })
    )
  }

  /* Proc */
  def visitDispatch( p : Proc, arg : A ) : R = {
    p match {
      case pNil : PNil => visit( pNil, arg )
      case pVal : PValue => visit( pVal, arg )
      case pDrop : PDrop => visit( pDrop, arg )
      case pInject : PInject => visit( pInject, arg )
      case pLift : PLift => visit( pLift, arg )
      case pFoldL : PFoldL => visit( pFoldL, arg )
      case pFoldR : PFoldR => visit( pFoldR, arg )
      case pInput : PInput => visit( pInput, arg )
      case pChoice : PChoice => visit( pChoice, arg )
      case pMatch : PMatch => visit( pMatch, arg )
      case pNew : PNew => visit( pNew, arg )
      case pConstr : PConstr => visit( pConstr, arg )
      case pContr : PContr => visit (pContr, arg)
      case pPrint : PPrint => visit (pPrint, arg)
      case pPar : PPar => visit( pPar, arg )
    }
  }
  override def visit(  p : PPrint, arg : A ) : R = {
    combine(
      arg,
      for(
        Location( pTerm : StrTermCtxt, _ ) <- visitDispatch( p.proc_, Here() )
      ) yield {
        val printTerm = B(_rx)(G("'print"), pTerm)
        val displayTerm = B(_rx)(G("'display"), G( "'#\\\\n"))
        L( B( _seq )( printTerm, displayTerm ), Top() )
      }
    )
  }
  override def visit(  p : PNil, arg : A ) : R = {    
    combine( arg, Some( L( G( "#niv" ), T() ) ) )
  }
  override def visit(  p : PValue, arg : A ) : R = {
    combine( arg, visitDispatch( p.value_, Here() ) )
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
    combine( arg, 
      ( p.chan_ match {
        case quote : CQuote => {
          visitDispatch( quote.proc_, Here() )
        }
        case v : CVar => {
          Some( L( B( _run )( B( _compile )( V( v.var_ ) ) ), Top() ) )
        }
      } )
    )    
  }
  override def visit(  p : PInject, arg : A ) : R

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
            visitDispatch( e, Here() ) match {
              case Some( Location( pTerm : StrTermCtxt, _ ) ) => {
                acc ++ List( pTerm )
              }
              case None => acc
            }
          }
        }
      )        

    combine(
      arg,
      for( Location( cTerm : StrTermCtxt, _ ) <- visitDispatch( p.chan_, Here() ) ) yield {
        L( B(_rx)( (List(G(_produce)) ++ List( TS ) ++ List(cTerm) ++ List(V(_wildcard)) ++ actls):_* ), Top() )
      }
    )
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
        Location(procTerm: StrTermCtxt, _) <- visitDispatch(p.proc_, Here())
      ) yield {
        val bindingsComponents = bindings map {
          case inBind: InputBind => {
            for (
              Location(chanTerm: StrTermCtxt, _) <- visitDispatch(inBind.chan_, Here());
              Location(ptrnTerm: StrTermCtxt, _) <- visitDispatch(inBind.cpattern_, Here())
            ) yield {
              var productFresh = V(s"""'${Fresh()}""")
              var unificationBindingFresh = V(s"""'${Fresh()}""")
              var wildcard = V(_wildcard)
              val quotedPtrnTerm = inBind.cpattern_ match {
                case _ => (for (Location(q: StrTermCtxt, _) <- doQuote(ptrnTerm)) yield {
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
        val consumeTerm = B(_rx)(G(_consume), TS, B(_list)(chanTerms: _*), B(_list)(wildcards: _*), B(_list)(quotedPtrnTerms: _*), G("'#f")) // #f for persistent
        val letBindingsTerm = B(_list)(B(_list)(B(_list)(unificationFreshes: _*), B(_list)(productFreshes: _*)), consumeTerm)
        val bodyTerm = B(_rx)(B(_abs)(B(_list)(B(_list)(ptrnTerms: _*)), procTerm), B(_list)(productFreshes: _*))
        L(B(_let)(B(_list)(letBindingsTerm), bodyTerm), T())
      }
    }

    combine(
      arg,
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
    )
  }

  override def visit(  p : PNew, arg : A ) : R = {
    import scala.collection.JavaConverters._
    val newVars = p.listvar_.asScala.toList
    combine(
      arg,
      (for( Location( pTerm : StrTermCtxt, _ ) <- visitDispatch( p.proc_, Here() ) )
        yield {
          val newBindings = newVars.map( { ( v ) => {
            val fresh = V(s"""'${Fresh()}""")
            val quotedFresh = (for (Location(q: StrTermCtxt, _) <- doQuote(fresh)) yield {
              q
            }).getOrElse(throw new FailedQuotation(fresh))
            B(_list)(V( s"""'${v}""" ), quotedFresh)
          } } )
          L( B(_let)( ( List(B(_list)(newBindings:_*)) ++ List( pTerm )):_* ), Top() )
        })
    )
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
        combine( arg, Some( L( G( "#niv" ), T() ) ) )
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
      val result = for (Location(pTerm: StrTermCtxt, _) <- visitDispatch(p.proc_, Here())) yield {
        val reverseListPMBranch = p.listpmbranch_.asScala.toList.reverse
        (nonExhaustiveMatch /: reverseListPMBranch) {
          (acc, e) => {
            e match {
              case pm: PatternMatch => {
                for (
                  Location(pattern: StrTermCtxt, _) <- visitDispatch(pm.ppattern_, Here());
                  Location(continuation: StrTermCtxt, _) <- visitDispatch(pm.proc_, Here());
                  Location(remainder: StrTermCtxt, _) <- acc
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

    combine(
      arg,
      patternMatchVisitAux
    )
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
            visitDispatch( e, Here() ) match {
              case Some( Location( pTerm : StrTermCtxt, _ ) ) => acc ++ List( pTerm )
              case None => acc
            }
          }
        }
      )        

    combine(
      arg,
      Some( L( B( p.var_ )( (List(V(s"""${p.var_}Contract""")) ++ actls):_* ), Top() ) )
    )
  }
  override def visit(  p : PPar, arg : A ) : R = {
    /*
     * [| P1 | P2 |]( t ) 
     * =
     * ( block [| P1 |]( t ) [| P2 |]( t ) )
     */
    combine(
      arg,
      for( 
        Location( pTerm1 : StrTermCtxt, _ ) <- visitDispatch( p.proc_1, Here() );
        Location( pTerm2 : StrTermCtxt, _ ) <- visitDispatch( p.proc_2, Here() )
      ) yield {
        L( B( _block )( pTerm1, pTerm2 ), Top() )
      }
    )
  }

  /* Chan */
  def visitDispatch(  p : Chan, arg : A ) : R = {
    p match {
      case cVar : CVar => visit( cVar, arg )
      case cQuote : CQuote => visit( cQuote, arg )
    }
  }
  override def visit(  p : CVar, arg : A ) : R = {
    combine( arg, Some( L( V( s"""'${p.var_}""" ), T() ) ) )
  }
  override def visit(  p : CQuote, arg : A ) : R = {
    // TODO: Handle quoting and unquoting
    combine( arg, visitDispatch( p.proc_, Here() ) )
  }
  /* Bind */
  def visit( b : Bind, arg : A ) : R
  override def visit(  p : InputBind, arg : A ) : R = {
    arg match {
      // [[ P ]] is proc
      case Some( Location( proc : StrTermCtxt, Top() ) ) => {
        for(
          // [[ chan ]] is chanTerm
          Location( chanTerm : StrTermCtxt, _ ) <- visitDispatch( p.chan_, Here() );
          // [[ ptrn ]] is ptrnTerm
          Location( ptrnTerm : StrTermCtxt, _ ) <- visitDispatch( p.cpattern_, Here() )
        ) yield {
          // ( map [[ chan ]] proc [[ ptrn ]] [[ P ]] )
          L( B( _map )( chanTerm, B( _abs )( B(_list)(ptrnTerm), proc ) ), T() )
        }
      }
      case _ => { // this is a little too optimistic or forgiving
        // should ensure that arg is a reasonable context to be really safe
        for(
          // [[ chan ]] is chanTerm
          Location( chanTerm : StrTermCtxt, _ ) <- visitDispatch( p.chan_, Here() );
          // [[ ptrn ]] is ptrnTerm
          Location( ptrnTerm : StrTermCtxt, _ ) <- visitDispatch( p.cpattern_, Here() );
          Location( rbindingsTerm : StrTermCtxt, _ ) <- arg
        ) yield {
          // ( flatMap [[ chan ]] proc [[ ptrn ]] [[ for( bindings )P ]] )
          L( B( _join )( chanTerm, B( _abs )( B(_list)(ptrnTerm), rbindingsTerm ) ), T() )
        }
      }
    }
  }
  /* CBranch */
  override def visit(  p : Choice, arg : A ) : R

  /* Value */
  def visitDispatch( p : Value, arg : A ) : R = {
    p match {
      case quant : VQuant => visit( quant, arg )
      case char : EChar => visit( char, arg )
      case tuple : ETuple => visit( tuple, arg )
    }
  }
  override def visit(  p : VQuant, arg : A ) : R = {
    combine( arg, visitDispatch( p.quantity_, Here() ) )
  }
  /* Quantity */
  def visitDispatch( p : Quantity, arg : A ) : R = {
    p match {
      case qVar : QVar => visit( qVar, arg )
      case bool : QBool => visitDispatch( bool.rhobool_, arg )
      case int : QInt => visit( int, arg )
      case double : QDouble => visit( double, arg )
      case string : QString => visit( string, arg )
      case map : QMap => visit( map, arg )

      case method : QDot => visit( method, arg )

      case neg : QNeg => visit(neg, arg)
      case mult : QMult => visit(mult, arg)
      case div : QDiv => visit(div, arg)
      case add : QAdd => visit(add, arg)
      case minus : QMinus => visit(minus, arg)

      case lt : QLt => visit(lt, arg)
      case lte : QLte => visit(lte, arg)
      case gt : QGt => visit(gt, arg)
      case gte : QGte => visit(gte, arg)
      case eq : QEq => visit(eq, arg)
      case neq : QNeq => visit(neq, arg)
    }
  }
  override def visit(  p : QVar, arg : A ) : R = {
    combine( arg, L(V(s"""'${p.var_}"""), Top()) )
  }
  def visitDispatch( p : RhoBool, arg : A ) : R = {
    p match {
      case qTrue : QTrue => visit( qTrue, arg )
      case qFalse : QFalse => visit( qFalse, arg )
    }
  }
  override def visit(  p : QInt, arg : A ) : R = {
    combine(
      arg,
      L( G( s"""${p.integer_}"""), Top() )
    )
  }
  override def visit(  p : QDouble, arg : A ) : R = {
    combine(
      arg,
      L( G( s"""${p.double_}"""), Top() )
    )
  }
  override def visit( p : QTrue, arg : A) : R = {
    combine(
      arg,
      L( G( s"""#t"""), Top() )
    )
  }
  override def visit( p : QFalse, arg : A) : R = {
    combine(
      arg,
      L(G( s"""#f"""), Top())
    )
  }
  override def visit(  p : QString, arg : A ) : R = {
    combine(
      arg,
      L( G( s""""${p.string_}"""" ), Top() )
    )
  }
  override def visit( p : QMap, arg : A) : R = {
    combine(
      arg,
      L(G( s"""(new RblTable)"""), Top())
    )
  }
  override def visit( p : QDot, arg : A) : R = {
    import scala.collection.JavaConverters._

    /* [[ quantity ]].method_name( [ [[ quantity_arg1 ]], [[ quantity_arg2 ]], ... ] )
     * =
     * (method_name quantity quantity_arg1 quantity_arg2)
     */
    combine(
      arg,
      for (Location( q : StrTermCtxt, _ ) <- visitDispatch( p.quantity_, Here() )) yield {
        val qArgs =
          ( List[StrTermCtxt]() /: p.listquantity_.asScala.toList )(
            {
              ( acc, e ) => {
                visitDispatch( e, Here() ) match {
                  case Some( Location( frml : StrTermCtxt, _ ) ) => {
                    acc ++ List( frml )
                  }
                  case None => {
                    acc
                  }
                }
              }
            }
          )
        L( B(p.var_)( (List(q) ++ qArgs):_* ), Top() )
      }
    )
  }
  override def visit( p : QNeg, arg : A) : R = {
    combine(
      arg,
      for( Location( q : StrTermCtxt, _ ) <- visitDispatch( p.quantity_, Here() ) ) yield {
        L( B("-")(q), Top() )
      }
    )
  }
  override def visit( p : QMult, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("*")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QDiv, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("/")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QAdd, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("+")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QLt, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("<")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QLte, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("<=")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QGt, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B(">")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QGte, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B(">=")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QEq, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("==")(q1,q2), Top() )
      }
    )
  }
  override def visit( p : QNeq, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("!=")(q1,q2), Top() )
      }
    )
  }


  override def visit( p : QMinus, arg : A) : R = {
    combine(
      arg,
      for(
        Location( q1 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_1, Here() );
        Location( q2 : StrTermCtxt, _ ) <- visitDispatch( p.quantity_2, Here() )
      ) yield {
        L( B("-")(q1,q2), Top() )
      }
    )
  }

  /* Entity */
  override def visit(  p : EChar, arg : A ) : R = {
    combine(
      arg,
      L( G( s"""'${p.char_}'"""), Top() )
    )
  }
  override def visit(  p : ETuple, arg : A ) : R = {
    import scala.collection.JavaConverters._
    val procTerms =
      ( List[StrTermCtxt]() /: p.listproc_.asScala.toList )(
        {
          ( acc, e ) => {
            visitDispatch( e, Here() ) match {
              case Some( Location( frml : StrTermCtxt, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )
    combine(
      arg,
      L( B(_list)( procTerms:_* ), Top() )
    )
  }

  /* Pattern */
  def visitDispatch( p : CPattern, arg : A ) : R = {
    p match {
      case cPtVar : CPtVar => visit( cPtVar, arg )
      case cPtQuote : CPtQuote => visit( cPtQuote, arg )
      case cValPtrn : CValPtrn => visit( cValPtrn, arg )
    }
  }
  /* VarPattern */
  def visitDispatch( p : VarPattern, arg : A ) : R = {
    p match {
      case regular: VarPtVar => visit(regular, Here())
      case wild: VarPtWild => visit(wild, Here())
    }
  }
  override def visit(p: VarPtVar, arg: A): R = {
    combine( arg, L(V(s"""'${p.var_}"""), Top()) )
  }
  override def visit(  p : VarPtWild, arg : A ) : R

  /* PPattern */
  def visitDispatch( p : PPattern, arg : A ) : R = {
    // TODO: Fill in rest of PPattern subclasses
    p match {
      case pPtVar : PPtVar => visit( pPtVar, arg )
      case pPtNil : PPtNil => visit( pPtNil, arg )
      case pPtVal : PPtVal => visit( pPtVal, arg )
    }
  }
  override def visit(  p : PPtVar, arg : A ) : R = {
    combine(arg, visitDispatch(p.varpattern_, Here()))
  }
  override def visit(  p : PPtNil, arg : A ) : R
  override def visit(  p : PPtVal, arg : A ) : R = {
    combine(arg, visitDispatch(p.valpattern_, Here()))
  }

  override def visit(  p : PPtDrop, arg : A ) : R
  override def visit(  p : PPtInject, arg : A ) : R

  override def visit(  p : PPtOutput, arg : A ) : R

  override def visit(  p : PPtInput, arg : A ) : R
  override def visit(  p : PPtMatch, arg : A ) : R
  override def visit(  p : PPtNew, arg : A ) : R
  override def visit(  p : PPtConstr, arg : A ) : R
  override def visit(  p : PPtPar, arg : A ) : R

  /* CPattern */
  override def visit(p: CPtVar, arg: A): R = {
    combine(arg, visitDispatch(p.varpattern_, Here()))
  }
  override def visit(  p : CPtQuote, arg : A ) : R
  /* PatternBind */
  override def visit(  p : PtBind, arg : A ) : R
  /* PatternPatternMatch */
  override def visit(  p : PtBranch, arg : A ) : R
  /* ValPattern */
  def visitDispatch( p : ValPattern, arg : A ) : R = {
    // TODO: Fill in rest of ValPattern subclasses
    p match {
      case vPtStruct : VPtStruct => visit( vPtStruct, arg )
      case vPtTuple : VPtTuple => visit (vPtTuple, arg)
      case vPtTrue : VPtTrue => visit( vPtTrue, arg )
      case vPtFalse : VPtFalse => visit( vPtFalse, arg )
      case vPtInt : VPtInt => visit( vPtInt, arg )
    }
  }
  override def visit(  p : VPtStruct, arg : A ) : R = {
    import scala.collection.JavaConverters._
    
    val structContents =
      ( List[StrTermCtxt]() /: p.listppattern_.asScala.toList )(
        {
          ( acc, e ) => {
            visitDispatch( e, Here() ) match {
              case Some( Location( frml : StrTermCtxt, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )

    combine(
      arg,
      L( B(p.var_)( structContents:_* ), Top() )
    )
  }
  override def visit(  p : VPtTuple, arg : A ) : R = {
    import scala.collection.JavaConverters._

    val tupleContents =
      ( List[StrTermCtxt]() /: p.listppattern_.asScala.toList )(
        {
          ( acc, e ) => {
            visitDispatch( e, Here() ) match {
              case Some( Location( frml : StrTermCtxt, _ ) ) => {
                acc ++ List( frml )
              }
              case None => {
                acc
              }
            }
          }
        }
      )

    combine(
      arg,
      L( B(_list)( tupleContents:_* ), Top() )
    )
  }
  override def visit(  p : VPtTrue, arg: A ): R = {
    combine(
      arg,
      L( G( s"""#t"""), Top() )
    )
  }
  override def visit(  p : VPtFalse, arg: A ): R = {
    combine(
      arg,
      L( G( s"""#f"""), Top() )
    )
  }
  override def visit(  p : VPtInt, arg: A ): R = {
    combine(
      arg,
      L( G( s"""${p.integer_}"""), Top() )
    )
  }
}
