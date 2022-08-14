This is java bindings to secp256k1 native libraries.

Native libraries are produced in https://github.com/rchain/secp256k1-native, and pulled from there by CI into 
resources folder.

The content of this module is taken from [bitcoin-core repo](https://github.com/bitcoin-core/secp256k1/pull/682),
which deprecated JNI support. Tests are adjusted to be run with Scalatest.