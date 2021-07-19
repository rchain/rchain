package com.revdefine.node.setup

import cats.syntax.all._
import cats.effect.{ConcurrentEffect, Timer}
import com.google.protobuf.ByteString
import com.revdefine.node.store.MongoStore
import com.revdefine.node.store.MongoStore.MongoOperateError
import com.revdefine.node.web.node.LastFinalizedBlock
import com.revdefine.node.web.transfer.Transfer.Transaction
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.dag.DagOps
import coop.rchain.shared.syntax._
import coop.rchain.shared.Log

import scala.concurrent.duration.DurationInt

object UpdateFinalized {

  def updateFinalized[F[_]: ConcurrentEffect: Log: Timer: EngineCell: BlockDagStorage](
      mongo: MongoStore[F]
  ): F[Unit] = {
    def finalizedBlock(
        lastTransaction: BlockMessage,
        lastFinalizedTransaction: Transaction,
        dagRe: BlockDagRepresentation[F]
    ) =
      for {
        _ <- Log[F].debug(
              s"Trying to finalized from ${lastFinalizedTransaction} to ${lastTransaction}."
            )
        unFinalizedTransactionBlock <- for {
                                        blockHashes <- dagRe.topoSort(
                                                        lastFinalizedTransaction.blockNumber + 1L,
                                                        Some(lastTransaction.body.state.blockNumber)
                                                      )
                                        flatBlocks = blockHashes.flatten
                                        _ <- Log[F].debug(
                                              s"There are ${flatBlocks.length} blocks " +
                                                s"between ${lastFinalizedTransaction.blockNumber} " +
                                                s"and ${lastTransaction.body.state.blockNumber}"
                                            )
                                        unfinalized <- flatBlocks.filterA(
                                                        b => dagRe.isFinalized(b)
                                                      )
                                      } yield unfinalized
        - <- Log[F].debug(s"Find ${unFinalizedTransactionBlock.length} blocks to be finalized.")
        _ <- unFinalizedTransactionBlock.traverse { b =>
              val finalizing =
                Log[F].debug(s"Started to finalized ${Base16.encode(b.toByteArray)}") >>
                  mongo.finalizedBlock(Base16.encode(b.toByteArray)) >> Log[F].debug(
                  s"Successfully finalized ${Base16.encode(b.toByteArray)}"
                )
              ConcurrentEffect[F].handleErrorWith(finalizing) {
                case e @ MongoOperateError(msg) =>
                  Log[F].error(msg + "\n" + e.getStackTrace.mkString("\n")) >> e.raiseError[F, Unit]
                case e @ _ => e.raiseError[F, Unit]
              }
            }
      } yield ()

    for {
      lastFinalizedBlock            <- LastFinalizedBlock.lastFinalizedBlock[F]
      _                             <- Log[F].debug(s"Last finalized block is ${lastFinalizedBlock}")
      lastFinalizedTransactionBlock <- mongo.lastFinalizedTransaction
      lastTransaction               <- mongo.lastTransaction
      _                             <- Log[F].debug(s"Last finalized transaction is ${lastFinalizedTransactionBlock}")
      dagRe                         <- BlockDagStorage[F].getRepresentation
      shouldFinalized = lastTransaction.blockNumber > lastFinalizedBlock.body.state.blockNumber &&
        lastFinalizedTransactionBlock.blockNumber < lastFinalizedBlock.body.state.blockNumber
      _ <- finalizedBlock(lastFinalizedBlock, lastFinalizedTransactionBlock, dagRe).whenA(
            shouldFinalized
          )
      _ <- Log[F]
            .debug(
              s"Should not finalized block because last finalized block : ${lastFinalizedBlock}, " +
                s"last transaction ${lastTransaction}, last finalized transaction ${lastFinalizedTransactionBlock}"
            )
            .whenA(!shouldFinalized)
      _ <- Timer[F].sleep(10.seconds)

    } yield ()
  }

}
