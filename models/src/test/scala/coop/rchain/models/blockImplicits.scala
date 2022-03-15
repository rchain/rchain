package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.crypto.{signatures, PrivateKey, PublicKey}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.shared.Base16
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOfN
import org.scalacheck.util.Buildable

import scala.util.Random

object blockImplicits {

  // Generators
  val blockHashGen: Gen[BlockHash] = for {
    byteArray <- listOfN(BlockHash.Length, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  val stateHashGen: Gen[StateHash] = for {
    byteArray <- listOfN(StateHash.Length, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  val validatorGen: Gen[Validator] = for {
    byteArray <- listOfN(Validator.Length, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  val bondGen: Gen[Bond] = for {
    byteArray <- listOfN(Validator.Length, arbitrary[Byte])
    validator = ByteString.copyFrom(byteArray.toArray)
    stake     <- Gen.chooseNum(1L, 1024L)
  } yield Bond(validator, stake)

  val justificationGen: Gen[Justification] = for {
    latestBlockHash <- arbitrary[BlockHash](Arbitrary(blockHashGen))
    byteArray       <- listOfN(Validator.Length, arbitrary[Byte])
    validator       = ByteString.copyFrom(byteArray.toArray)
  } yield Justification(validator, latestBlockHash)

  def signedDeployDataGen(shardId: String): Gen[Signed[DeployData]] =
    for {
      termLength <- Gen.choose(32, 1024)
      term       <- listOfN(termLength, Gen.alphaNumChar).map(_.mkString)
      timestamp  <- arbitrary[Long]
      (sec, _)   = Secp256k1.newKeyPair
      deployData = Signed(
        DeployData(
          term = term,
          timestamp = timestamp,
          phloLimit = 90000,
          phloPrice = 1L,
          validAfterBlockNumber = 0,
          shardId = shardId
        ),
        signatures.Secp256k1,
        sec
      )
    } yield deployData

  def processedDeployGen(shardId: String): Gen[ProcessedDeploy] =
    for {
      deployData <- signedDeployDataGen(shardId)
    } yield ProcessedDeploy(
      deploy = deployData,
      cost = PCost(0L),
      deployLog = List.empty,
      isFailed = false
    )

  // Arbitrary values
  val arbitraryBlockHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)
  val arbitraryBond: Arbitrary[Bond]           = Arbitrary(bondGen)
  val arbitraryStateHash: Arbitrary[StateHash] = Arbitrary(stateHashGen)
  val arbitraryValidator: Arbitrary[Validator] = Arbitrary(validatorGen)

  val arbitraryJustification: Arbitrary[Justification] = Arbitrary(
    justificationGen
  )
  def arbitraryProcessedDeploy(shardId: String): Arbitrary[ProcessedDeploy] = Arbitrary(
    processedDeployGen(shardId)
  )
  val arbitraryJustifications: Arbitrary[Seq[Justification]] =
    Arbitrary.arbContainer[Seq, Justification](
      arbitraryJustification,
      Buildable.buildableCanBuildFrom,
      identity
    )
  def arbitraryProcessedDeploys(shardId: String): Arbitrary[Seq[ProcessedDeploy]] =
    Arbitrary.arbContainer[Seq, ProcessedDeploy](
      arbitraryProcessedDeploy(shardId),
      Buildable.buildableCanBuildFrom,
      identity
    )
  val arbitraryBlockHashes: Arbitrary[Seq[BlockHash]] =
    Arbitrary.arbContainer[Seq, BlockHash](
      arbitraryBlockHash,
      Buildable.buildableCanBuildFrom,
      identity
    )
  val arbitraryBonds: Arbitrary[Seq[Bond]] =
    Arbitrary.arbContainer[Seq, Bond](
      arbitraryBond,
      Buildable.buildableCanBuildFrom,
      identity
    )

  val blockElementsGen: Gen[List[BlockMessage]] =
    Gen.listOf(blockElementGen())

  val blockBatchesGen: Gen[List[List[BlockMessage]]] =
    Gen.listOf(blockElementsGen)

  def blockElementGen(
      setBlockNumber: Option[Long] = Some(0L),
      setSeqNumber: Option[Int] = Some(0),
      setPreStateHash: Option[StateHash] = None,
      setPostStateHash: Option[StateHash] = None,
      setValidator: Option[Validator] = None,
      setVersion: Option[Long] = None,
      setTimestamp: Option[Long] = None,
      setParentsHashList: Option[Seq[BlockHash]] = None,
      setJustifications: Option[Seq[Justification]] = None,
      setDeploys: Option[Seq[ProcessedDeploy]] = None,
      setSysDeploys: Option[Seq[ProcessedSystemDeploy]] = None,
      setBonds: Option[Seq[Bond]] = None,
      setShardId: Option[String] = None,
      hashF: Option[BlockMessage => BlockHash] = None
  ): Gen[BlockMessage] =
    for {
      preStatehash <- if (setPreStateHash.isEmpty)
                       arbitrary[StateHash](arbitraryStateHash)
                     else Gen.const(setPreStateHash.get)
      postStatehash <- if (setPostStateHash.isEmpty)
                        arbitrary[StateHash](arbitraryStateHash)
                      else Gen.const(setPostStateHash.get)
      parentsHashList <- if (setParentsHashList.isEmpty)
                          arbitrary[Seq[BlockHash]](arbitraryBlockHashes)
                        else Gen.const(setParentsHashList.get)
      justifications <- if (setJustifications.isEmpty)
                         arbitrary[Seq[Justification]](arbitraryJustifications)
                       else Gen.const(setJustifications.get)
      shardId = if (setShardId.isEmpty) "root" else setShardId.get
      deploys <- if (setDeploys.isEmpty)
                  arbitrary[Seq[ProcessedDeploy]](arbitraryProcessedDeploys(shardId))
                else Gen.const(setDeploys.get)
      // 10 random validators in bonds list
      bonds <- if (setBonds.isEmpty) Gen.containerOfN[List, Bond](10, bondGen)
              else Gen.const(setBonds.get)
      // Pick random validator from bonds file
      validator <- if (setValidator.isEmpty)
                    Gen.const(
                      Random.shuffle(bonds).headOption.getOrElse(bondGen.sample.get).validator
                    )
                  else Gen.const(setValidator.get)
      version   = if (setVersion.isEmpty) 1L else setVersion.get
      timestamp <- if (setTimestamp.isEmpty) arbitrary[Long] else Gen.const(setTimestamp.get)
      block = BlockMessage(
        blockHash = ByteString.EMPTY,
        header = Header(
          parentsHashList = parentsHashList.toList,
          timestamp = timestamp,
          version = version
        ),
        body = Body(
          state = RChainState(
            preStateHash = preStatehash,
            postStateHash = postStatehash,
            bonds = bonds.toList,
            blockNumber = setBlockNumber.get
          ),
          deploys = deploys.toList,
          systemDeploys = setSysDeploys.toList.flatten,
          rejectedDeploys = List.empty
        ),
        justifications = justifications.toList,
        sender = validator,
        seqNum = setSeqNumber.get,
        sig = ByteString.EMPTY,
        sigAlgorithm = "",
        shardId = shardId
      )
      blockHash <- if (hashF.isEmpty) arbitrary[BlockHash](arbitraryBlockHash)
                  else Gen.const(hashF.get(block))
      ret = block.copy(blockHash = blockHash)
    } yield ret

  def blockElementsWithParentsGen(genesis: BlockMessage): Gen[List[BlockMessage]] =
    Gen.sized { size =>
      (0 until size).foldLeft(Gen.listOfN(0, blockElementGen())) {
        case (gen, _) =>
          for {
            blocks       <- gen
            b            <- blockElementGen(setBonds = Some(genesis.body.state.bonds))
            parents      <- Gen.someOf(blocks)
            parentHashes = parents.map(_.blockHash).toList
            newBlock     = b.copy(header = b.header.copy(parentsHashList = parentHashes))
          } yield newBlock :: blocks
      }
    }

  def blockWithNewHashesGen(blockElements: List[BlockMessage]): Gen[List[BlockMessage]] =
    Gen.listOfN(blockElements.size, blockHashGen).map { blockHashes =>
      blockElements.zip(blockHashes).map {
        case (b, hash) => b.copy(blockHash = hash)
      }
    }

  def getRandomBlock(
      setBlockNumber: Option[Long] = Some(0L),
      setSeqNumber: Option[Int] = Some(0),
      setPreStateHash: Option[StateHash] = None,
      setPostStateHash: Option[StateHash] = None,
      setValidator: Option[Validator] = None,
      setVersion: Option[Long] = None,
      setTimestamp: Option[Long] = None,
      setParentsHashList: Option[Seq[BlockHash]] = None,
      setJustifications: Option[Seq[Justification]] = None,
      setDeploys: Option[Seq[ProcessedDeploy]] = None,
      setSysDeploys: Option[Seq[ProcessedSystemDeploy]] = None,
      setBonds: Option[Seq[Bond]] = None,
      setShardId: Option[String] = None,
      hashF: Option[BlockMessage => BlockHash] = None
  ): BlockMessage =
    blockElementGen(
      setBlockNumber,
      setSeqNumber,
      setPreStateHash,
      setPostStateHash,
      setValidator,
      setVersion,
      setTimestamp,
      setParentsHashList,
      setJustifications,
      setDeploys,
      setSysDeploys,
      setBonds,
      setShardId,
      hashF
    ).sample.get
}
