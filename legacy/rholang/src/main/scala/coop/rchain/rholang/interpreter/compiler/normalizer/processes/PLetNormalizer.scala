package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.{EList, Match, MatchCase, Par, Var}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  FreeMap,
  NameVisitInputs,
  NameVisitOutputs,
  ProcNormalizeMatcher,
  ProcVisitInputs,
  ProcVisitOutputs,
  VarSort
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.normalizer.{
  NameNormalizeMatcher,
  RemainderNormalizeMatcher
}

import scala.collection.immutable.BitSet
import java.util.UUID
import scala.collection.convert.ImplicitConversionsToScala._
object PLetNormalizer {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def normalize[F[_]: Sync](p: PLet, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    p.decls_ match {

      case concDeclsImpl: ConcDeclsImpl =>
        def extractNamesAndProcs(decl: Decl): (ListName, NameRemainder, ListProc) =
          decl match {
            case declImpl: DeclImpl =>
              (declImpl.listname_, declImpl.nameremainder_, declImpl.listproc_)
          }

        val (listNames, listNameRemainders, listProcs) =
          (extractNamesAndProcs(p.decl_) :: concDeclsImpl.listconcdecl_.toList.map {
            case concDeclImpl: ConcDeclImpl => extractNamesAndProcs(concDeclImpl.decl_)
          }).unzip3

        /*
         It is not necessary to use UUIDs to achieve concurrent let declarations.
         While there is the possibility for collisions with either variables declared by the user
         or variables declared within this translation, the chances for collision are astronomically
         small (see analysis here: https://towardsdatascience.com/are-uuids-really-unique-57eb80fc2a87).
         A strictly correct approach would be one that performs a ADT rather than an AST translation, which
         was not done here due to time constraints.
         */
        val variableNames = List.fill(listNames.size)(UUID.randomUUID().toString)

        val psends = variableNames.zip(listProcs).map {
          case (variableName, listProc) =>
            new PSend(new NameVar(variableName), new SendSingle(), listProc)
        }

        val pinput = {
          val listLinearBind = new ListLinearBind()
          variableNames
            .zip(listNames)
            .zip(listNameRemainders)
            .map {
              case ((variableName, listName), nameRemainder) =>
                new LinearBindImpl(
                  listName,
                  nameRemainder,
                  new SimpleSource(new NameVar(variableName))
                )
            }
            .foreach(listLinearBind.add)
          val listReceipt = new ListReceipt()
          listReceipt.add(new ReceiptLinear(new LinearSimple(listLinearBind)))
          new PInput(listReceipt, p.proc_)
        }

        val ppar = {
          val procs = psends :+ pinput
          procs.drop(2).foldLeft(new PPar(procs.head, procs(1))) {
            case (ppar, proc) => new PPar(ppar, proc)
          }
        }

        val pnew = {
          val listNameDecl = new ListNameDecl()
          variableNames.map(new NameDeclSimpl(_)).foreach(listNameDecl.add)
          new PNew(listNameDecl, ppar)
        }

        normalizeMatch[F](pnew, input)

      /*
      Let processes with a single bind or with sequential binds ";" are converted into match processes rather
      than input processes, so that each sequential bind doesn't add a new unforgeable name to the tuplespace.
      The Rholang 1.1 spec defines them as the latter. Because the Rholang 1.1 spec defines let processes in terms
      of a output process in concurrent composition with an input process, the let process appears to quote the
      process on the RHS of "<-" and bind it to the pattern on LHS. For example, in
          let x <- 1 in { Nil }
      the process (value) "1" is quoted and bound to "x" as a name. There is no way to perform an AST transformation
      of sequential let into a match process and still preserve these semantics, so we have to do an ADT transformation.
       */
      case _ =>
        val newContinuation =
          p.decls_ match {
            case _: EmptyDeclImpl => p.proc_
            case linearDeclsImpl: LinearDeclsImpl =>
              val newDecl =
                linearDeclsImpl.listlineardecl_.head match {
                  case impl: LinearDeclImpl => impl.decl_
                }
              val newDecls =
                if (linearDeclsImpl.listlineardecl_.size == 1)
                  new EmptyDeclImpl()
                else {
                  val newListLinearDecls = new ListLinearDecl()
                  linearDeclsImpl.listlineardecl_.tail.foreach(newListLinearDecls.add)
                  new LinearDeclsImpl(newListLinearDecls)
                }
              new PLet(newDecl, newDecls, p.proc_)
          }

        def listProcToEList(
            listProc: List[Proc],
            knownFree: FreeMap[VarSort]
        ): F[ProcVisitOutputs] =
          listProc
            .foldM((Vector.empty[Par], knownFree, BitSet.empty, false)) {
              case ((vectorPar, knownFree, locallyFree, connectiveUsed), proc) =>
                ProcNormalizeMatcher
                  .normalizeMatch[F](
                    proc,
                    ProcVisitInputs(VectorPar(), input.boundMapChain, knownFree)
                  )
                  .map {
                    case ProcVisitOutputs(par, updatedKnownFree) =>
                      (
                        par +: vectorPar,
                        updatedKnownFree,
                        locallyFree | par.locallyFree,
                        connectiveUsed | par.connectiveUsed
                      )
                  }
            }
            .map {
              case (vectorPar, knownFree, locallyFree, connectiveUsed) =>
                ProcVisitOutputs(
                  EList(vectorPar.reverse, locallyFree, connectiveUsed, none[Var]),
                  knownFree
                )
            }

        // Largely similar to how consume patterns are processed.
        def listNameToEList(
            listName: List[Name],
            nameRemainder: NameRemainder
        ): F[ProcVisitOutputs] =
          RemainderNormalizeMatcher
            .normalizeMatchName(nameRemainder, FreeMap.empty[VarSort]) >>= {
            case (optionalVar, remainderKnownFree) =>
              listName
                .foldM((Vector.empty[Par], remainderKnownFree, BitSet.empty)) {
                  case ((vectorPar, knownFree, locallyFree), name) =>
                    NameNormalizeMatcher
                      .normalizeMatch[F](
                        name,
                        NameVisitInputs(input.boundMapChain.push, knownFree)
                      )
                      .map {
                        case NameVisitOutputs(par, updatedKnownFree) =>
                          (
                            par +: vectorPar,
                            updatedKnownFree,
                            // Use input.env.depth + 1 because the pattern was evaluated w.r.t input.env.push,
                            // and more generally because locally free variables become binders in the pattern position
                            locallyFree | ParLocallyFree
                              .locallyFree(par, input.boundMapChain.depth + 1)
                          )
                      }
                }
                .map {
                  case (vectorPar, knownFree, locallyFree) =>
                    ProcVisitOutputs(
                      EList(vectorPar.reverse, locallyFree, connectiveUsed = true, optionalVar),
                      knownFree
                    )
                }
          }

        p.decl_ match {
          case declImpl: DeclImpl =>
            listProcToEList(declImpl.listproc_.toList, input.freeMap) >>= {
              case ProcVisitOutputs(valueListPar, valueKnownFree) =>
                listNameToEList(declImpl.listname_.toList, declImpl.nameremainder_) >>= {
                  case ProcVisitOutputs(patternListPar, patternKnownFree) =>
                    normalizeMatch[F](
                      newContinuation,
                      ProcVisitInputs(
                        VectorPar(),
                        input.boundMapChain.absorbFree(patternKnownFree),
                        valueKnownFree
                      )
                    ).map {
                      case ProcVisitOutputs(continuationPar, continuationKnownFree) =>
                        ProcVisitOutputs(
                          input.par.prepend(
                            Match(
                              target = valueListPar,
                              cases = Seq(
                                MatchCase(
                                  patternListPar,
                                  continuationPar,
                                  patternKnownFree.countNoWildcards
                                )
                              ),
                              locallyFree = valueListPar.locallyFree | patternListPar.locallyFree | continuationPar.locallyFree
                                .from(patternKnownFree.countNoWildcards)
                                .map(_ - patternKnownFree.countNoWildcards),
                              connectiveUsed = valueListPar.connectiveUsed || continuationPar.connectiveUsed
                            )
                          ),
                          continuationKnownFree
                        )
                    }
                }
            }
        }
    }

}
