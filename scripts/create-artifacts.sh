#! /usr/bin/env bash

set -euo pipefail

if [ $SUBPROJECT = "core" ]
then
    sbt -Dsbt.log.noformat=true clean node/debian:packageBin node/rpm:packageBin
fi
