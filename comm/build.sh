#!/usr/bin/env sh

# we have to run compile and then test because of the issue
# still not resolved in https://github.com/scalapb/ScalaPB/issues/400
sbt compile &>/dev/null
sbt test
