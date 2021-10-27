package coop.rchain.rholang.interpreter.storage

import cats.FlatMap
import cats.effect.Concurrent
import cats.implicits._
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.syntax._
import coop.rchain.rholang.interpreter.{PrettyPrinter, RhoRuntime}
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.Base16

import scala.collection.SortedSet

object StoragePrinter {
  val noUnmatchedSends = "No unmatched sends."

  def prettyPrint[F[_]: FlatMap](runtime: RhoRuntime[F]): F[String] =
    for {
      mapped <- runtime.getHotChanges
      pars = mapped.map {
        case (
            channels: Seq[Par],
            row: Row[BindPattern, ListParWithRandom, TaggedContinuation]
            ) => {
          row match {
            case Row(Nil, Nil) =>
              Par()
            case Row(data: Seq[Datum[ListParWithRandom]], Nil) =>
              toSends(data)(channels)
            case Row(
                Nil,
                wks: Seq[WaitingContinuation[BindPattern, TaggedContinuation]]
                ) =>
              toReceive(wks)(channels)
            case Row(
                data: Seq[Datum[ListParWithRandom]],
                wks: Seq[WaitingContinuation[BindPattern, TaggedContinuation]]
                ) =>
              toSends(data)(channels) ++ toReceive(wks)(channels)
          }
        }
      }
    } yield {
      if (pars.isEmpty)
        "The space is empty. Note that top level terms that are not sends or receives are discarded."
      else
        PrettyPrinter().buildString(pars.reduce(_ ++ _))
    }

  def prettyPrintUnmatchedSends[F[_]: FlatMap](runtime: RhoRuntime[F]): F[String] =
    runtime.getHotChanges.map(getUnmatchedSends).map { unmatchedSends =>
      if (unmatchedSends.isEmpty)
        noUnmatchedSends
      else
        PrettyPrinter().buildString(unmatchedSends.reduce(_ ++ _))
    }

  def prettyPrintUnmatchedSends[F[_]: Concurrent](
      deploy: Signed[DeployData],
      runtime: RhoRuntime[F]
  ): F[String] = {
    def unmatchedSends: F[List[Par]] = runtime.getHotChanges.map(getUnmatchedSends)
    for {
      checkpoint <- runtime.createCheckpoint
      beforeEval <- unmatchedSends
      _ <- {
        runtime.evaluate(deploy.data.term, NormalizerEnv(deploy).toEnv)
      }
      afterEval <- unmatchedSends
      diff      = afterEval.diff(beforeEval)
      _         <- runtime.reset(checkpoint.root)
    } yield {
      if (diff.isEmpty)
        noUnmatchedSends
      else
        PrettyPrinter().buildString(diff.reduce(_ ++ _))
    }
  }

  def prettyPrintUnmatchedSends[F[_]: Concurrent](
      deploys: Seq[Signed[DeployData]],
      runtime: RhoRuntime[F]
  ): F[String] =
    deploys.toStream
      .traverse(
        deploy =>
          prettyPrintUnmatchedSends(deploy, runtime)
            .map(unmatchedSends => (deploy, unmatchedSends))
      )
      .map(
        list =>
          list
            .filter(_._2 != noUnmatchedSends)
            .map {
              case (deployData, unmatchedSends) =>
                s"Deploy ${Base16.encode(deployData.sig.toByteArray)}:\n" + unmatchedSends
            }
            .mkString("\n\n")
      )

  private def getUnmatchedSends(
      mapped: Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]
  ): List[Par] =
    mapped.map {
      case (
          channels: Seq[Par],
          row: Row[BindPattern, ListParWithRandom, TaggedContinuation]
          ) =>
        row match {
          case Row(data: Seq[Datum[ListParWithRandom]], _) =>
            toSends(data)(channels)
        }
    }.toList

  private[this] def toSends(data: Seq[Datum[ListParWithRandom]])(channels: Seq[Par]): Par = {
    val sends: Seq[Send] = data.flatMap {
      case Datum(as: ListParWithRandom, persist: Boolean, _: Produce) =>
        channels.map { channel =>
          Send(channel, as.pars, persist)
        }
    }
    sends.foldLeft(Par()) { (acc: Par, send: Send) =>
      acc.prepend(send)
    }
  }

  private[this] def toReceive(
      wks: Seq[WaitingContinuation[BindPattern, TaggedContinuation]]
  )(channels: Seq[Par]): Par = {
    val receives: Seq[Receive] = wks.map {
      case WaitingContinuation(
          patterns: Seq[BindPattern],
          continuation: TaggedContinuation,
          persist: Boolean,
          peeks: SortedSet[Int],
          _: Consume
          ) =>
        val receiveBinds: Seq[ReceiveBind] = (channels zip patterns).map {
          case (channel, pattern) =>
            ReceiveBind(
              pattern.patterns,
              channel,
              pattern.remainder,
              pattern.freeCount
            )
        }
        continuation.taggedCont match {
          case ParBody(p) =>
            Receive(
              receiveBinds,
              p.body,
              persist,
              peeks.nonEmpty,
              patterns.map(_.freeCount).sum
            )
          case _ => Receive(receiveBinds, Par.defaultInstance, persist)
        }
    }
    receives.foldLeft(Par()) { (acc: Par, receive: Receive) =>
      acc.prepend(receive)
    }
  }
}
