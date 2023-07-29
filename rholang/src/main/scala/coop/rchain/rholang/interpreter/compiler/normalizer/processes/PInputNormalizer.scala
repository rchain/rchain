package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.models.{Par, ReceiveBind}
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler._
import coop.rchain.rholang.interpreter.compiler.normalizer.processes.Utils.failOnInvalidConnective
import coop.rchain.rholang.interpreter.compiler.normalizer.{
  NameNormalizeMatcher,
  RemainderNormalizeMatcher
}
import coop.rchain.rholang.interpreter.errors.{
  ReceiveOnSameChannelsError,
  UnexpectedReuseOfNameContextFree
}

import java.util.UUID
import scala.jdk.CollectionConverters._

object PInputNormalizer {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def normalize[F[_]: Sync](p: PInput, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {
    if (p.listreceipt_.size() > 1) {
      normalizeMatch[F](
        p.listreceipt_.asScala.reverse.foldLeft(p.proc_) { (proc, receipt) =>
          val listReceipt = new ListReceipt()
          listReceipt.add(receipt)
          new PInput(listReceipt, proc)
        },
        input
      )
    } else {

      val receiptContainsComplexSource: Boolean = {
        p.listreceipt_.asScala.head match {
          case rl: ReceiptLinear =>
            rl.receiptlinearimpl_ match {
              case ls: LinearSimple =>
                ls.listlinearbind_.asScala.exists {
                  case lbi: LinearBindImpl =>
                    lbi.namesource_ match {
                      case _: SimpleSource => false
                      case _               => true
                    }

                }
              case _ => false
            }
          case _ => false
        }
      }

      if (receiptContainsComplexSource) {
        p.listreceipt_.asScala.head match {
          case rl: ReceiptLinear =>
            rl.receiptlinearimpl_ match {
              case ls: LinearSimple =>
                val listReceipt    = new ListReceipt()
                val listLinearBind = new ListLinearBind()
                val listNameDecl   = new ListNameDecl()
                listReceipt.add(new ReceiptLinear(new LinearSimple(listLinearBind)))
                val (sends, continuation) =
                  ls.listlinearbind_.asScala.foldLeft((new PNil: Proc, p.proc_)) {
                    case ((sends, continuation), lb) =>
                      lb match {
                        case lbi: LinearBindImpl =>
                          lbi.namesource_ match {
                            case _: SimpleSource =>
                              listLinearBind.add(lbi)
                              (sends, continuation)
                            case _ =>
                              val identifier = UUID.randomUUID().toString
                              val r          = new NameVar(identifier)
                              lbi.namesource_ match {
                                case rss: ReceiveSendSource =>
                                  lbi.listname_.asScala.prepend(r)
                                  listLinearBind.add(
                                    new LinearBindImpl(
                                      lbi.listname_,
                                      lbi.nameremainder_,
                                      new SimpleSource(rss.name_)
                                    )
                                  )
                                  (
                                    sends,
                                    new PPar(
                                      new PSend(r, new SendSingle, new ListProc()),
                                      continuation
                                    )
                                  )
                                case srs: SendReceiveSource =>
                                  listNameDecl.add(new NameDeclSimpl(identifier))
                                  listLinearBind.add(
                                    new LinearBindImpl(
                                      lbi.listname_,
                                      lbi.nameremainder_,
                                      new SimpleSource(r)
                                    )
                                  )
                                  srs.listproc_.asScala.prepend(new PEval(r))
                                  (
                                    new PPar(
                                      new PSend(srs.name_, new SendSingle, srs.listproc_),
                                      sends
                                    ): Proc,
                                    continuation
                                  )
                              }
                          }
                      }
                  }
                val pInput = new PInput(listReceipt, continuation)
                normalizeMatch[F](
                  if (listNameDecl.isEmpty) pInput
                  else new PNew(listNameDecl, new PPar(sends, pInput)),
                  input
                )
            }

        }
      } else {

        // To handle the most common case where we can sort the binds because
        // they're from different sources, Each channel's list of patterns starts its free variables at 0.
        // We check for overlap at the end after sorting. We could check before, but it'd be an extra step.
        // We split this into parts. First we process all the sources, then we process all the bindings.
        def processSources(
            sources: Vector[Name]
        ): F[(Vector[ParN], FreeMap[VarSort])] =
          sources.foldM((Vector.empty[ParN], input.freeMap)) {
            case ((vectorPar, knownFree), name) =>
              NameNormalizeMatcher
                .normalizeMatch[F](name, NameVisitInputs(input.boundMapChain, knownFree))
                .map {
                  case NameVisitOutputs(par, knownFree) =>
                    (
                      vectorPar :+ fromProto(par),
                      knownFree
                    )
                }
          }

        def processPatterns(
            patterns: Vector[(Vector[Name], NameRemainder)]
        ): F[Vector[(Vector[ParN], Option[VarN], FreeMap[VarSort])]] =
          patterns.traverse {
            case (names, nameRemainder) =>
              names
                .foldM((Vector.empty[ParN], FreeMap.empty[VarSort])) {
                  case ((vectorPar, knownFree), name) =>
                    NameNormalizeMatcher
                      .normalizeMatch[F](
                        name,
                        NameVisitInputs(input.boundMapChain.push, knownFree)
                      ) >>= {
                      case nameVisitOutputs @ NameVisitOutputs(par, knownFree) =>
                        failOnInvalidConnective(input, nameVisitOutputs)
                          .fold(
                            _.raiseError[F, (Vector[ParN], FreeMap[VarSort])],
                            _ => (vectorPar :+ fromProto(par), knownFree).pure[F]
                          )
                    }
                } >>= {
                case (vectorPar, knownFree) =>
                  RemainderNormalizeMatcher.normalizeMatchName(nameRemainder, knownFree).map {
                    case (optionalVar, knownFree) =>
                      (vectorPar, fromProtoVarOpt(optionalVar), knownFree)
                  }
              }
          }

        // If we get to this point, we know p.listreceipt.size() == 1
        val (consumes, persistent, peek) =
          p.listreceipt_.asScala.head match {
            case rl: ReceiptLinear =>
              rl.receiptlinearimpl_ match {
                case ls: LinearSimple =>
                  (ls.listlinearbind_.asScala.toVector.map {
                    case lbi: LinearBindImpl =>
                      ((lbi.listname_.asScala.toVector, lbi.nameremainder_), lbi.namesource_ match {
                        // all sources should be simple sources by this point
                        case ss: SimpleSource => ss.name_
                      })
                  }, false, false)
              }
            case rr: ReceiptRepeated =>
              rr.receiptrepeatedimpl_ match {
                case rs: RepeatedSimple =>
                  (rs.listrepeatedbind_.asScala.toVector.map {
                    case rbi: RepeatedBindImpl =>
                      ((rbi.listname_.asScala.toVector, rbi.nameremainder_), rbi.name_)
                  }, true, false)

              }
            case rp: ReceiptPeek =>
              rp.receiptpeekimpl_ match {
                case ps: PeekSimple =>
                  (ps.listpeekbind_.asScala.toVector.map {
                    case pbi: PeekBindImpl =>
                      ((pbi.listname_.asScala.toVector, pbi.nameremainder_), pbi.name_)
                  }, false, true)

              }

          }

        val (patterns, names) = consumes.unzip

        def fromReceiveBind(x: ReceiveBind): ReceiveBindN = {
          val patterns  = fromProto(x.patterns)
          val source    = fromProto(x.source)
          val remainder = fromProtoVarOpt(x.remainder)
          val freeCount = x.freeCount
          ReceiveBindN(patterns, source, remainder, freeCount)
        }

        for {
          processedSources       <- processSources(names)
          (sources, sourcesFree) = processedSources
          processedPatterns      <- processPatterns(patterns)
          receiveBindsAndFreeMaps <- ReceiveBindsSortMatcher.preSortBinds[F, VarSort](
                                      processedPatterns.zip(sources).map {
                                        case ((a, b, c), e) =>
                                          (toProto(a), toProtoVarOpt(b), toProto(e), c)
                                      }
                                    )
          unz                                 = receiveBindsAndFreeMaps.unzip
          (receiveBinds, receiveBindFreeMaps) = (unz._1.map(fromReceiveBind), unz._2)
          channels                            = receiveBinds.map(_.source)
          hasSameChannels                     = channels.size > channels.toSet.size
          _ <- ReceiveOnSameChannelsError(p.line_num, p.col_num)
                .raiseError[F, Unit]
                .whenA(hasSameChannels)
          receiveBindsFreeMap <- receiveBindFreeMaps.toList.foldM(FreeMap.empty[VarSort]) {
                                  case (knownFree, receiveBindFreeMap) =>
                                    knownFree.merge(receiveBindFreeMap) match {
                                      case (updatedKnownFree, Nil) => updatedKnownFree.pure[F]
                                      case (_, (shadowingVar, sourcePosition) :: _) =>
                                        UnexpectedReuseOfNameContextFree(
                                          shadowingVar,
                                          knownFree.get(shadowingVar).get.sourcePosition,
                                          sourcePosition
                                        ).raiseError[F, FreeMap[VarSort]]
                                    }
                                }
          procVisitOutputs <- normalizeMatch[F](
                               p.proc_,
                               ProcVisitInputs(
                                 NilN(),
                                 input.boundMapChain.absorbFree(receiveBindsFreeMap),
                                 sourcesFree
                               )
                             )
        } yield {
          val bindCount = receiveBindsFreeMap.countNoWildcards
          val receive =
            ReceiveN(receiveBinds, procVisitOutputs.par, persistent, peek, bindCount)
          ProcVisitOutputs(
            input.par.add(receive),
            procVisitOutputs.freeMap
          )
        }
      }
    }
  }
}
