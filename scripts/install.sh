#!/usr/bin/env bash

set -e

if [ -f "${SUBPROJECT}/install.sh" ]; then
    (cd "${SUBPROJECT}"; ./install.sh)
else
    git clone https://github.com/BNFC/bnfc.git
    git clone https://github.com/bitcoin-core/secp256k1
    cd bnfc/source
    sudo cabal install --global
    cd ../../secp256k1
    ./autogen.sh
    ./configure --enable-jni --enable-experimental --enable-module-schnorr --enable-module-ecdh --prefix=$PWD/../rchain/crypto/src/main/resources
    make install
fi
