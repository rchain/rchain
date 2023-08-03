package coop.rchain.models.rholangn

final class UPrivateN(val v: Array[Byte]) extends UnforgeableN
object UPrivateN { def apply(bytes: Array[Byte]): UPrivateN = new UPrivateN(bytes) }

final class UDeployIdN(val v: Array[Byte]) extends UnforgeableN
object UDeployIdN { def apply(bytes: Array[Byte]): UDeployIdN = new UDeployIdN(bytes) }

final class UDeployerIdN(val v: Array[Byte]) extends UnforgeableN
object UDeployerIdN { def apply(bytes: Array[Byte]): UDeployerIdN = new UDeployerIdN(bytes) }

// TODO: Temporary solution for easier conversion from old types - change type in the future
final class USysAuthTokenN(val v: Array[Byte]) extends UnforgeableN
object USysAuthTokenN { def apply(): USysAuthTokenN = new USysAuthTokenN(Array[Byte]()) }
