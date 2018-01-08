#!/usr/bin/env bash

RWORK=/Users/kent/Documents/
RHOLANG_DIR=$RWORK/Rholang
RHOLANG_WEB_DIR=$RWORK/RholangREPL

# Be sure to build the jar first:
# ln -s .../rchain; cd rchain/rholang; sbt assembly
JAR=rchain/rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar

java -jar $JAR test.rho



