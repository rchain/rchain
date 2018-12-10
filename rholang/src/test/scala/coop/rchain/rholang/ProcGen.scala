package coop.rchain.rholang
import cats.Invariant
import cats.data.NonEmptyList
import coop.rchain.rholang.syntax.rholang_mercury.Absyn._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Shrink._

import scala.collection.JavaConverters
import scala.reflect.{classTag, ClassTag}
import GenTools._

object tools {
  def seqToJavaCollection[C <: java.util.Collection[T]: ClassTag, T](input: Seq[T]): C = {
    val r = classTag[C].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[C]
    input.foreach(r.add)
    r
  }

  def javaCollectionToSeq[C <: java.util.Collection[T], T](col: C): Seq[T] =
    JavaConverters.asScalaIterator(col.iterator()).toSeq

  def streamSingleton[T](v: T): Stream[T] = v #:: Stream.empty[T]

  def withQuotes(quote: Char)(s: String): String = quote + s + quote
  val stringQuotes: String => String             = withQuotes('"')
  val uriQuotes: String => String                = withQuotes('`')

  def withoutQuotes(s: String): String = {
    assert(s.length >= 2)
    s.substring(1, s.length - 1)
  }

  def mkUri(components: Seq[String]): String = components.mkString(":")

  val uriGen: Gen[String] =
    for {
      componentCount <- Gen.choose(1, 10)
      components     <- Gen.listOfN(componentCount, nonemptyString(10))
    } yield uriQuotes(mkUri(components))

  def extractNames(seqNameDecl: Seq[NameDecl]): Set[String] =
    seqNameDecl.map {
      case simple: NameDeclSimpl => simple.var_
      case urn: NameDeclUrn      => urn.var_
    }.toSet

  def extractFreeVariables(localNames: Set[String], p: Proc): Set[String] =
    p match {
      case par: PPar =>
        extractFreeVariables(localNames, par.proc_1) ++ extractFreeVariables(localNames, par.proc_2)
      case v: PVar =>
        v.procvar_ match {
          case vv: ProcVarVar if !localNames.contains(vv.var_) => Set(vv.var_)
          case _                                               => Set.empty[String]
        }
      case n: PNew =>
        val newNames = extractNames(javaCollectionToSeq[ListNameDecl, NameDecl](n.listnamedecl_))
        extractFreeVariables(localNames ++ newNames, n.proc_)

      case _ => Set.empty[String]
    }
}

object ProcGen {
  import tools._

  private case class State(height: Int, boundNames: Set[String] = Set.empty[String]) {
    def decrementHeight: State                   = this.copy(height = height - 1)
    def addNames(names: Iterable[String]): State = this.copy(boundNames = boundNames ++ names)
  }

  private def pparGen(state: State): Gen[PPar] = {
    val newState = state.decrementHeight
    for {
      p1 <- procGen(processContextProcs, newState)
      p2 <- procGen(processContextProcs, newState)
    } yield new PPar(p1, p2)
  }

  private def pgroundGen(state: State): Gen[Proc] = {
    val groundIntGen = arbitrary[Long]
      .map((n: Long) => {
        if (n > 0)
          new PGround(new GroundInt(n.toString))
        else new PNeg(new PGround(new GroundInt((-n).toString)))
      })
    val groundBoolGen =
      Gen.oneOf(new BoolFalse(), new BoolTrue()).map(b => new PGround(new GroundBool(b)))
    val groundStringGen =
      Arbitrary.arbString.arbitrary.map(s => new PGround(new GroundString(stringQuotes(s))))
    val groundUriGen =
      uriGen.map(s => new PGround(new GroundUri(s)))

    Gen.oneOf(
      groundIntGen,
      groundBoolGen,
      groundStringGen,
      groundUriGen
    )
  }

  private def nameQuoteGen(state: State): Gen[NameQuote] =
    procGen(processContextProcs, state.decrementHeight).map(new NameQuote(_))

  private def nameGen(state: ProcGen.State): Gen[Name] =
    nameQuoteGen(state)

  private lazy val sendGen: Gen[Send] =
    Gen.oneOf(
      Gen.const(new SendSingle()),
      Gen.const(new SendMultiple())
    )

  private def psendGen(state: State): Gen[PSend] = {
    val newState = state.decrementHeight
    for {
      name     <- nameGen(newState)
      send     <- sendGen
      listProc <- Gen.listOf(procGen(processContextProcs, newState))
    } yield new PSend(name, send, seqToJavaCollection[ListProc, Proc](listProc))
  }

  private def pevalGen(state: State): Gen[PEval] =
    nameGen(state).map(new PEval(_))

  private lazy val pnilGen: Gen[PNil] = Gen.const(new PNil())

  private def nameDeclSimplGen: Gen[NameDeclSimpl] = identifierGen.map(new NameDeclSimpl(_))

  private lazy val nameDeclUrnGen: Gen[NameDeclUrn] =
    for {
      name <- identifierGen
      uri  <- uriGen
    } yield new NameDeclUrn(name, uri)

  private lazy val nameDeclGen: Gen[NameDecl] = Gen.oneOf(nameDeclSimplGen, nameDeclUrnGen)

  private def pnewGen(state: State): Gen[PNew] = {
    val newState = state.decrementHeight

    for {
      seqNameDecl <- Gen.nonEmptyListOf(nameDeclGen)
      names       = extractNames(seqNameDecl)
      proc        <- procGen(processContextProcs, newState.addNames(names))
    } yield new PNew(seqToJavaCollection[ListNameDecl, NameDecl](seqNameDecl), proc)
  }

  private def procGen(procGens: NonEmptyList[State => Gen[Proc]], state: State): Gen[Proc] =
    if (state.height > 0) {
      val newState = state.decrementHeight
      oneOf(procGens.map(_.apply(newState)))
    } else
      pnilGen

  private val processContextProcs: NonEmptyList[State => Gen[Proc]] =
    NonEmptyList.of(pgroundGen, pparGen, psendGen, pnewGen, pevalGen)

  def topLevelGen(height: Int): Gen[Proc] =
    procGen(processContextProcs, State(height))

  private val uriShrinker: Shrink[String] = Shrink { x: String =>
    {
      val components = x.split(":")

      for {
        shrinkedComponentSeq <- shrinkContainer[Seq, String]
                                 .shrink(components)
                                 .takeWhile(_.nonEmpty)
        shrinkedComponents <- shrinkedComponentSeq.map(c => shrinkString.shrink(c))
      } yield mkUri(shrinkedComponents)
    }
  }

  private val invariantFunctorShrink = new Invariant[Shrink] {
    override def imap[A, B](fa: Shrink[A])(f: A => B)(g: B => A): Shrink[B] =
      Shrink(b => fa.shrink(g(b)).map(f))
  }

  private val groundIntShrinker = invariantFunctorShrink.imap(shrinkIntegral[Long])(
    i => new GroundInt(i.toString)
  )(gi => gi.longliteral_.toLong)

  private val groundStringShrinker = invariantFunctorShrink.imap(shrinkString)(
    s => new GroundString(stringQuotes(s))
  )(gs => withoutQuotes(gs.stringliteral_))

  private val groundUriShrinker = invariantFunctorShrink.imap(uriShrinker)(
    s => new GroundUri(uriQuotes(s))
  )(gu => withoutQuotes(gu.uriliteral_))

  implicit def procShrinker: Shrink[Proc] = Shrink {
    case p: PGround =>
      (p.ground_ match {
        case p: GroundInt    => groundIntShrinker.shrink(p)
        case p: GroundBool   => streamSingleton(p)
        case p: GroundString => groundStringShrinker.shrink(p)
        case p: GroundUri    => groundUriShrinker.shrink(p)
      }).map(new PGround(_))

    case p: PPar =>
      for {
        sp1 <- shrink(p.proc_1)
        sp2 <- shrink(p.proc_2)
      } yield new PPar(sp1, sp2)

    case p: PSend =>
      val initialProcs = javaCollectionToSeq[ListProc, Proc](p.listproc_)

      shrinkContainer[Seq, Proc]
        .shrink(initialProcs)
        .takeWhile(_.nonEmpty)
        .map(procs => new PSend(p.name_, p.send_, seqToJavaCollection[ListProc, Proc](procs)))

    case p: PNew =>
      val initialNames = javaCollectionToSeq[ListNameDecl, NameDecl](p.listnamedecl_).toList

      def shrinkListNameDecl(initialNames: Seq[NameDecl]) =
        shrinkContainer[Seq, NameDecl].shrink(initialNames).takeWhile(_.nonEmpty)

      def hasNoFreeVars(localVars: Set[String], p: Proc) = {
        val freeVars = extractFreeVariables(localVars, p)

        freeVars.isEmpty
      }

      for {
        newNames     <- shrinkListNameDecl(initialNames)
        newLocalVars = extractNames(newNames)
        proc         <- shrink(p.proc_).filter(hasNoFreeVars(newLocalVars, _))
      } yield new PNew(seqToJavaCollection[ListNameDecl, NameDecl](newNames), proc)

    case p: PVar =>
      (p.procvar_ match {
        case v: ProcVarVar => shrinkString.shrink(v.var_).map(new ProcVarVar(_))
      }).map(new PVar(_))

    case p: PNeg =>
      shrink(p.proc_).map(new PNeg(_))

    case p: PNil => streamSingleton(p)

    case p: PEval =>
      (p.name_ match {
        case nv: NameVar   => streamSingleton(nv)
        case nq: NameQuote => shrink(nq.proc_).map(new NameQuote(_))
      }).map(new PEval(_))
  }
}
