#!/usr/bin/env bash

RWORK=/Users/kent/Documents/
RHOLANG_DIR=$RWORK/Rholang
RHOLANG_WEB_DIR=$RWORK/RholangREPL

cd $RHOLANG_DIR
sbt "run-main coop.rchain.rho2rose.Rholang2RosetteCompiler ${RHOLANG_WEB_DIR}/test.rho" > $RHOLANG_WEB_DIR/output.txt
cp $RHOLANG_WEB_DIR/output.txt $RHOLANG_WEB_DIR/original_output.txt
sed -n '/\[0m\[[32msuccess[0m\]/!p' $RHOLANG_WEB_DIR/output.txt > $RHOLANG_WEB_DIR/output2.txt
sed -n '/\[0m\[[0minfo[0m\]/!p' $RHOLANG_WEB_DIR/output2.txt > $RHOLANG_WEB_DIR/output3.txt
cat $RHOLANG_WEB_DIR/output3.txt


