#!/bin/sh

set -e

cd crypto
git clone https://github.com/jedisct1/libsodium --branch stable
cd libsodium
./configure --prefix=$HOME/libsodium
make && make check
sudo make install
