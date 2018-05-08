#!/usr/bin/env bash

set -e

if [ -f "${SUBPROJECT}/install.sh" ]; then
    (cd "${SUBPROJECT}"; ./install.sh)
else
    ./scripts/install_bnfc.sh
fi
