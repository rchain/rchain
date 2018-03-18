#!/bin/sh

set -e

cd crypto
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
./autogen.sh
./configure --enable-jni --enable-experimental --enable-module-schnorr --enable-module-ecdh
make
