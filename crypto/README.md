# RChain Cryptography module

`crypto` module provides cryptography functionalities for `node`.

## Before you build

In order to be able to build `crypto` module, you need to have [The Sodium crypto library](https://github.com/jedisct1/libsodium) installed on your system. For details see [libsodium documentation](https://download.libsodium.org/doc/installation/) where different installation options are described

## Available functionality

| Feature        | Description                               |
| -------------  |:-----------------------------------------:| 
| Base16         | Traditional hexadecimal String encoding   |
| Curve25519     | Elliptic curve cryptography               |
| Sha256         | Sha256 hashing algorithm                  |
| Keccak256      | Keccak256 hashing algorithm               |
| Blake2b256     | Blake2b256 hashing algorithm              |
| Ed25519        | Edwards-curve Digital Signature Algorithm |
