#! /bin/sh

set -e

if [ ! -d "$HOME/libsodium/lib" ]; then
    git clone https://github.com/jedisct1/libsodium.git --branch=stable
    cd libsodium
    ./configure --prefix=$HOME/libsodium
    make
    make install
else
    echo 'Using cached directory.'
fi
