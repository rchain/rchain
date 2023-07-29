package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.compiler.normalizer.{
  NameNormalizeMatcher,
  RemainderNormalizeMatcher
}

import java.util.UUID
import scala.jdk.CollectionConverters._

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
          (extractNamesAndProcs(p.decl_) :: concDeclsImpl.listconcdecl_.asScala.toList.map {
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
                linearDeclsImpl.listlineardecl_.asScala.head match {
                  case impl: LinearDeclImpl => impl.decl_
                }
              val newDecls =
                if (linearDeclsImpl.listlineardecl_.size == 1)
                  new EmptyDeclImpl()
                else {
                  val newListLinearDecls = new ListLinearDecl()
                  linearDeclsImpl.listlineardecl_.asScala.tail.foreach(newListLinearDecls.add)
                  new LinearDeclsImpl(newListLinearDecls)
                }
              new PLet(newDecl, newDecls, p.proc_)

          }

        def listProcToEList(
            listProc: List[Proc],
            knownFree: FreeMap[VarSort]
        ): F[ProcVisitOutputs] =
          listProc
            .foldM((Vector.empty[ParN], knownFree)) {
              case ((vectorPar, knownFree), proc) =>
                ProcNormalizeMatcher
                  .normalizeMatch[F](
                    proc,
                    ProcVisitInputs(NilN(), input.boundMapChain, knownFree)
                  )
                  .map {
                    case ProcVisitOutputs(par, updatedKnownFree) =>
                      (
                        par +: vectorPar,
                        updatedKnownFree
                      )
                  }
            }
            .map {
              case (vectorPar, knownFree) =>
                ProcVisitOutputs(
                  EListN(vectorPar.reverse, none),
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
                .foldM((Vector.empty[ParN], remainderKnownFree)) {
                  case ((vectorPar, knownFree), name) =>
                    NameNormalizeMatcher
                      .normalizeMatch[F](
                        name,
                        NameVisitInputs(input.boundMapChain.push, knownFree)
                      )
                      .map {
                        case NameVisitOutputs(par, updatedKnownFree) =>
                          (
                            fromProto(par) +: vectorPar,
                            updatedKnownFree
                          )
                      }
                }
                .map {
                  case (vectorPar, knownFree) =>
                    ProcVisitOutputs(
                      EListN(vectorPar.reverse, fromProtoVarOpt(optionalVar)),
                      knownFree
                    )
                }
          }

        p.decl_ match {
          case declImpl: DeclImpl =>
            listProcToEList(declImpl.listproc_.asScala.toList, input.freeMap) >>= {
              case ProcVisitOutputs(valueListPar, valueKnownFree) =>
                listNameToEList(declImpl.listname_.asScala.toList, declImpl.nameremainder_) >>= {
                  case ProcVisitOutputs(patternListPar, patternKnownFree) =>
                    normalizeMatch[F](
                      newContinuation,
                      ProcVisitInputs(
                        NilN(),
                        input.boundMapChain.absorbFree(patternKnownFree),
                        valueKnownFree
                      )
                    ).map {
                      case ProcVisitOutputs(continuationPar, continuationKnownFree) =>
                        val m = MatchN(
                          target = valueListPar,
                          cases = Seq(
                            MatchCaseN(
                              patternListPar,
                              continuationPar,
                              patternKnownFree.countNoWildcards
                            )
                          )
                        )
                        ProcVisitOutputs(input.par.add(m), continuationKnownFree)
                    }
                }
            }
        }
    }

}
