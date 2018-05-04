#! /usr/bin/env bash

set -eo pipefail

if [ "$SUBPROJECT" = "core" -a -n "$TRAVIS_TAG" ]
then
    sbt -Dsbt.log.noformat=true clean \
        rholang/bnfc:generate \
        node/debian:packageBin \
        node/rpm:packageBin \
        node/universal:packageZipTarball
fi
