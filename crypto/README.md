# RChain Cryptography module

`crypto` module provides cryptography functionalities for `node`.

## Before you build it

In order to be able to build `crypto` module, you need to have [The Sodium crypto library](https://github.com/jedisct1/libsodium) installed on your system. For details see [libsodium documentation](https://download.libsodium.org/doc/installation/) where different installation options are described

## Available functionality

| Feature                                                                       | Description                               |
| ----------------------------------------------------------------------------  | ----------------------------------------- | 
| [Base16](./src/main/scala/coop/rchain/crypto/codec/Base16.scala)              | Traditional hexadecimal String encoding   |
| [Curve25519](./src/main/scala/coop/rchain/crypto/encryption/Curve25519.scala) | Elliptic curve cryptography               |
| [Sha256](./src/main/scala/coop/rchain/crypto/hash/Sha256.sclaa)               | Sha256 hashing algorithm                  |
| [Keccak256](./src/main/scala/coop/rchain/crypto/hash/Keccak256.scala)         | Keccak256 hashing algorithm               |
| [Blake2b256](./src/main/scala/coop/rchain/crypto/hash/Blake2b256.scala)       | Blake2b256 hashing algorithm              |
| [Ed25519](./src/main/scala/coop/rchain/crypto/signatures/Ed25519.scala)       | Edwards-curve Digital Signature Algorithm |
