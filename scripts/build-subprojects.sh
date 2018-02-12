#!/bin/bash

if [ -f "build.sbt" ]; then
    echo "Building at top-level"
    sbt -Dsbt.log.noformat=true < "scripts/sbt-subprojects"
else
    echo "No build/test files found!"
    exit 1
fi
