package com.revdefine.node.web.node

import cats.syntax.all._
import cats.effect.{Concurrent, ContextShift, Sync}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.Par
import coop.rchain.shared.Log

import java.nio.file.{Files, Path}

object api {
  final case class API[F[_]: Concurrent: EngineCell: Log: SafetyOracle: BlockStore](
      oldDeployIdMap: Map[String, String]
  ) {
    def getBlock(
        blockHash: String
    ): F[BlockMessage] = BlockApi.getBlock(blockHash)

    def getBlocks(
        depth: Int
    ): F[Vector[BlockMessage]] = BlockApi.getBlocks(depth).flatMap(s => s.compile.to(Vector))

    def getBlocksByHeights(
        startBlockNumber: Long,
        endBlockNumber: Long
    ): F[Vector[BlockMessage]] =
      BlockApi
        .getBlocksByHeights(startBlockNumber, endBlockNumber)
        .flatMap(s => s.compile.to(Vector))

    def exploreDeploy(
        term: String,
        blockHash: String,
        usePreStateHash: Boolean
    ): F[Seq[Par]] = ExploreDeploy.exploreDeploy(term, blockHash, usePreStateHash)

    def findDeploy(
        deployId: String
    ): F[BlockMessage] = FindDeploy.findDeploy(deployId, oldDeployIdMap)

    def isFinalized(
        blockHash: String
    ): F[Boolean] = IsFinalized.isFinalized(blockHash)

    def lastFinalizedBlock: F[BlockMessage] = LastFinalizedBlock.lastFinalizedBlock
  }

  def create[F[_]: Concurrent: EngineCell: Log: SafetyOracle: BlockStore](
      oldDeployIdMap: Map[String, String]
  ): API[F] = API(oldDeployIdMap)

  def createAPI[F[_]: Concurrent: Log: ContextShift: EngineCell: BlockStore: SafetyOracle](
      dataDir: Path
  ): F[API[F]] = {
    import fs2.io
    import fs2.text
    import cats.effect.Blocker
    import cats.syntax.all._
    val oldDeployId = dataDir.resolve("revdefine/deployId.txt")
    val oldDeployIdMap = Blocker[F].use { b =>
      io.file
        .readAll[F](oldDeployId, b, chunkSize = 4096)
        .through(text.utf8Decode)
        .through(text.lines)
        .filter(_.trim.nonEmpty)
        .evalMap(
          line =>
            line.split(",") match {
              case Array(deployId, blockHash) => (deployId, blockHash).pure
              case _ =>
                Sync[F]
                  .raiseError[(String, String)](
                    new Exception(s"Old deploy index is not right ${oldDeployId}")
                  )
            }
        )
        .compile
        .to(Map)
    }

    val DeployIDMap =
      if (Files.exists(oldDeployId)) oldDeployIdMap else Map.empty[String, String].pure
    DeployIDMap.map(api.create[F])
  }
}
