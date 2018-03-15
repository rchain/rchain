#! /usr/bin/env bash

set -euo pipefail

SBT_OPTS="${SBT_OPTS:-} -Dsbt.log.noformat=true"

sbt ${SBT_OPTS} node/debian:packageBin node/rpm:packageBin
