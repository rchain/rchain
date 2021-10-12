package coop.rchain.casper.deploychainsetcasper
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.deploychainsetcasper.DeployChainSetCasper._
import coop.rchain.casper.protocol.{BlockMessage, DeployChain, StateMetadata}
import coop.rchain.v2.casper.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.v2.casper.stcasper.{StCasper, StateMessage}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueTypedStore

case class DeployChainSetCasper[F[_]: Sync](stateStore: CasperStateStore[F])
    extends StCasper[F, StateMessageData, DeployChain, Validator] {
  override def justifications(message: StateMessageData): F[List[StateMessageData]] =
    stateStore.getUnsafe(message.id).flatMap(m => stateStore.get(m.justifications).map(_.flatten))
  override def parents(message: StateMessageData): F[scala.List[StateMessageData]]  =
    stateStore.getUnsafe(message.id).flatMap(m => stateStore.get(m.parents).map(_.flatten))
  override def sender(message: StateMessageData): Validator                         = message.sender
  override def seqNum(message: StateMessageData): Int                               = message.seqNum
  override def bondsMap(message: StateMessageData): Map[Validator, Long]            = message.bondsMap
}

object DeployChainSetCasper {

  /**
   * Data required for Casper.
   */
  case class StateMessageData(
      id: BlockHash,
      justifications: List[BlockHash],
      parents: List[BlockHash],
      sender: Validator,
      stateMetadata: StateMetadata,
      seqNum: Int,
      bondsMap: Map[Validator, Long] // Todo this won't change much
  )

  /**
   * Representation of StateMessageData as a state message. Required for StateCasperImpl to extend StCasper.
   */
  def metaAsState(data: StateMessageData): StateMessage[DeployChain] =
    new StateMessage[DeployChain] {
      override def proposed                                = data.stateMetadata.proposed.toSet
      override def merged: ConflictResolution[DeployChain] =
        ConflictResolution(
          data.stateMetadata.acceptedSet.toSet,
          data.stateMetadata.rejectedSet.toSet
        )
    }

  implicit val metaOrd: Ordering[StateMessageData] = Ordering.by[StateMessageData, BlockHash](_.id)

  type CasperStateStore[F[_]] = KeyValueTypedStore[F, BlockHash, StateMessageData]

  /**
   * Add block to Casper state
   */
  def addBlock[F[_]](b: BlockMessage, store: CasperStateStore[F]): F[Unit] = ???

}
