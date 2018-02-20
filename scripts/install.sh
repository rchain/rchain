#!/usr/bin/env bash

set -e

if [ -f "${SUBPROJECT}/install.sh" ]; then
    (cd "${SUBPROJECT}"; ./install.sh)
else
    git clone https://github.com/BNFC/bnfc.git
    cd bnfc/source
    sudo cabal install --global
fi