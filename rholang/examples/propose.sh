#!/usr/bin/env bash
set -ex

RNODE="../../node/target/universal/stage/bin/rnode"

sed -e '' $3 $2 > $2.run

$RNODE deploy --phlo-limit 10000000000000 --phlo-price 1 --private-key $1 $2.run
$RNODE propose

rm $2.run
