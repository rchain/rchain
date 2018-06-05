#! /usr/bin/env bash

set -eo pipefail

if [ "$SUBPROJECT" = "core" -a -n "$TRAVIS_TAG" ]
then
    sbt -Dsbt.log.noformat=true clean \
        rholang/bnfc:generate \
        deployment/debian:packageBin \
        deployment/rpm:packageBin \
        deployment/universal:packageZipTarball
fi
