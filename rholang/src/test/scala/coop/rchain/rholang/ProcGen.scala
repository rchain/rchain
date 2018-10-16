package coop.rchain.rholang
import coop.rchain.rholang.syntax.rholang_mercury.Absyn._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalacheck.Shrink._

import scala.collection.JavaConverters
import scala.reflect.{classTag, ClassTag}

object tools {
  def seqToJavaCollection[C <: java.util.Collection[T]: ClassTag, T](input: Seq[T]): C = {
    val r = classTag[C].runtimeClass.newInstance().asInstanceOf[C]
    input.foreach(r.add)
    r
  }

  def javaCollectionToSeq[C <: java.util.Collection[T], T](col: C): Seq[T] =
    JavaConverters.asScalaIterator(col.iterator()).toSeq

  def streamSingleton[T](v: T): Stream[T] = v #:: Stream.empty[T]

  def nonemptyString(g: Gen[Char], size: Int): Gen[String] = Gen.nonEmptyListOf(g).map(_.mkString)

  def oneOf[T](gs: Seq[Gen[T]]): Gen[T] =
    if (gs.nonEmpty)
      Gen.choose(0, gs.size - 1).flatMap(gs(_))
    else
      throw new IllegalArgumentException("oneOf called on empty generator collection")

  def subsetOf[T](items: Seq[T]): Gen[Seq[T]] =
    for {
      count  <- Gen.choose(1, items.length)
      output <- Gen.pick(count, items)
    } yield output

  def mkGroundUri(components: Seq[String]): String = components.mkString(":")

  val uriGen: Gen[String] =
    for {
      componentCount <- Gen.choose(1, 10)
      components     <- Gen.listOfN(componentCount, nonemptyString(Gen.alphaChar, 10))
    } yield mkGroundUri(components)

  val identifierGen: Gen[String] = nonemptyString(Gen.alphaChar, 256)

  def extractNames(listNameDecl: ListNameDecl): List[String] = {
    var names = Set.empty[String]
    listNameDecl.forEach(
      name =>
        name.accept(
          new NameDecl.Visitor[Unit, Unit] {
            override def visit(p: NameDeclSimpl, arg: Unit): Unit = names = names + p.var_
            override def visit(p: NameDeclUrn, arg: Unit): Unit   = names = names + p.var_
          },
          ()
        )
    )
    names.toList
  }
}

object ProcGen {
  import tools._

  case class State(height: Int, usedNames: Set[String]) {
    def decrementHeight: State              = this.copy(height = height - 1)
    def addNames(names: Seq[String]): State = this.copy(usedNames = usedNames ++ names)
  }

  private def pparGen(state: State): Gen[PPar] =
    for {
      p1 <- procGen(topLevelProcs, state.decrementHeight)
      p2 <- procGen(topLevelProcs, state.decrementHeight)
    } yield new PPar(p1, p2)

  private def pgroundGen(state: State): Gen[Proc] = {
    lazy val groundIntGen = arbitrary[Long]
      .map((n: Long) => {
        if (n > 0)
          new PGround(new GroundInt(n.toString))
        else new PNeg(new PGround(new GroundInt((-n).toString)))
      })
    lazy val groundBoolGen =
      Gen.oneOf(new BoolFalse(), new BoolTrue()).map(b => new PGround(new GroundBool(b)))
    lazy val groundStringGen =
      Arbitrary.arbString.arbitrary.map(s => new PGround(new GroundString(s)))
    lazy val groundUriGen =
      uriGen.map(s => new PGround(new GroundUri(s)))

    Gen.oneOf(
      groundIntGen,
      groundBoolGen,
      groundStringGen,
      groundUriGen
    )
  }

  private def nameQuoteGen(state: State): Gen[NameQuote] =
    procGen(topLevelProcs, state.decrementHeight).map(new NameQuote(_))
  private def nameGen(state: State): Gen[Name] =
    nameQuoteGen(state)

  lazy val sendSingleGen: Gen[SendSingle]     = Gen.const(new SendSingle())
  lazy val sendMultipleGen: Gen[SendMultiple] = Gen.const(new SendMultiple())
  lazy val sendGen: Gen[Send] = Gen.oneOf(
    sendSingleGen,
    sendMultipleGen
  )

  private def listProcGen(state: State): Gen[ListProc] =
    Gen
      .listOf(procGen(topLevelProcs, state.decrementHeight))
      .map(seqToJavaCollection[ListProc, Proc])

  private def psendGen(state: State): Gen[PSend] =
    for {
      name     <- nameGen(state.decrementHeight)
      send     <- sendGen
      listProc <- listProcGen(state.decrementHeight)
    } yield new PSend(name, send, listProc)

  lazy val pnilGen: Gen[PNil] = Gen.const(new PNil())

  private def nameDeclSimplGen: Gen[NameDeclSimpl] = identifierGen.map(new NameDeclSimpl(_))

  lazy val nameDeclUrnGen: Gen[NameDeclUrn] =
    for {
      name <- identifierGen
      uri  <- uriGen
    } yield new NameDeclUrn(name, uri)

  lazy val nameDeclGen: Gen[NameDecl] = Gen.oneOf(nameDeclSimplGen, nameDeclUrnGen)

  private def listNameDeclGen(state: State): Gen[ListNameDecl] =
    Gen
      .nonEmptyListOf(nameDeclSimplGen)
      .map(seqToJavaCollection[ListNameDecl, NameDecl])

  private def pnewGen(state: State): Gen[PNew] = {
    val newState = state.decrementHeight

    for {
      listNameDecl <- listNameDeclGen(state.decrementHeight)
      strings      = extractNames(listNameDecl)
      proc         <- procGen(topLevelProcs, newState.addNames(strings))
    } yield new PNew(listNameDecl, proc)
  }

  lazy val bundleGen: Gen[Bundle] =
    Gen.oneOf(new BundleWrite(), new BundleRead(), new BundleReadWrite(), new BundleEquiv())

  private def procGen(procGens: Seq[State => Gen[Proc]], state: State): Gen[Proc] =
    if (state.height > 0) {
      oneOf(procGens.map(_.apply(state.decrementHeight)))
    } else
      pnilGen

  private def topLevelProcs: Seq[State => Gen[Proc]] =
    Seq(pgroundGen, pparGen, psendGen, pnewGen)

  def topLevelGen(height: Int): Gen[Proc] =
    procGen(topLevelProcs, State(height, Set.empty))

  implicit def procShrinker: Shrink[Proc] = Shrink {
    case p: PGround =>
      (p.ground_ match {
        case p: GroundInt =>
          shrinkIntegral[Long]
            .shrink(p.longliteral_.toLong)
            .map(n => new GroundInt(n.toString))
        case p: GroundBool => streamSingleton(p)
        case p: GroundString =>
          shrinkString
            .shrink(p.stringliteral_)
            .map(new GroundString(_))
        case p: GroundUri =>
          val components = p.uriliteral_.split(":")

          for {
            shrinkedComponentSeq <- shrinkContainer[Seq, String].shrink(components)
            shrinkedComponents   <- shrinkedComponentSeq.map(c => shrinkString.shrink(c))
          } yield new GroundUri(mkGroundUri(shrinkedComponents))
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
        .map(procs => new PSend(p.name_, p.send_, seqToJavaCollection[ListProc, Proc](procs)))

    case p: PNew =>
      val initialNames = javaCollectionToSeq[ListNameDecl, NameDecl](p.listnamedecl_)

      for {
        names <- shrinkContainer[Seq, NameDecl].shrink(initialNames)
        proc  <- shrink(p.proc_)
      } yield new PNew(seqToJavaCollection[ListNameDecl, NameDecl](names), proc)
  }
}
