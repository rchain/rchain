package com.revdefine.node.setup

import cats.syntax.all._
import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import com.revdefine.node.store.MongoStore
import com.revdefine.node.web.node.BlockApi
import com.revdefine.node.web.transfer.Transfer.{fromRnodeTransaction, Transaction}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.web.{CacheTransactionAPI, TransactionAPI}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log

import scala.concurrent.duration.DurationInt

object UpdateTransaction {

  def updateTransaction[F[_]: ConcurrentEffect: Timer: Log: ContextShift: EngineCell: BlockStore: SafetyOracle](
      mongo: MongoStore[F],
      transactionAPI: CacheTransactionAPI[F]
  ): F[Unit] = {
    def getTransaction(lastTransaction: Transaction, latestBlock: BlockMessage) = {
      val startBlock = lastTransaction.blockNumber + 1L
      for {
        _ <- Log[F].debug(
              s"Trying to get transactions from ${startBlock} to ${latestBlock.body.state.blockNumber}"
            )
        unKnownTransactionBlock <- BlockApi.getBlocksByHeights[F](
                                    lastTransaction.blockNumber + 1L,
                                    latestBlock.body.state.blockNumber
                                  )
        getTransactionStream = unKnownTransactionBlock.evalMap { b =>
          for {
            _ <- Log[F].debug(
                  s"Getting transactions from ${Base16.encode(b.blockHash.toByteArray)}"
                )
            transactions <- transactionAPI
                             .getTransaction(
                               Base16.encode(b.blockHash.toByteArray)
                             )
                             .map(_.data)
            _ <- Log[F].debug(
                  s"Successfully got ${transactions.length} transactions from ${Base16.encode(b.blockHash.toByteArray)}"
                )
            newTransfers = transactions.map(fromRnodeTransaction(_, b, isFinalized = false))
            _            <- mongo.insertTransaction(newTransfers)
            _ <- Log[F].debug(
                  s"Successfully insert ${transactions.length} transactions from ${Base16.encode(b.blockHash.toByteArray)} into mongo"
                )
          } yield ()
        }
        _ <- getTransactionStream.compile.toList
      } yield ()
    }

    for {
      latestBlockStream <- BlockApi.getBlocks[F](1)
      latestBlock       <- latestBlockStream.compile.toList.map(_.head)
      _                 <- Log[F].debug(s"Get the latest block ${latestBlock}")
      lastTransaction   <- mongo.lastTransaction
      _                 <- Log[F].debug(s"Get the latest transaction ${lastTransaction}")
      _ <- getTransaction(lastTransaction, latestBlock).whenA(
            latestBlock.body.state.blockNumber - lastTransaction.blockNumber >= 1L
          )
      _ <- Timer[F].sleep(10.seconds)
    } yield ()
  }

}
