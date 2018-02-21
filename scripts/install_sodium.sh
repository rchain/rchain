#! /bin/sh

set -e

cd crypto
git clone https://github.com/jedisct1/libsodium.git
cd libsodium
./configure --prefix=$HOME/libsodium
make
