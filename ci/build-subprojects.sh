#!/usr/bin/env bash
set -e

case "$SUBPROJECT" in "rosette")

    cd rosette
    nix-build

    ./run.sh rbl/rosette/tests/simple_add.rbl
    ;;

"core")

    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate coverage test coverageReport

    for sub in crypto comm rholang roscala storage node
    do
        (bash <(curl -s https://codecov.io/bash) -X gcov -s ./$sub -c -F $sub)
    done
    ;;

"test_artifact_creation")

    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/rpm:packageBin node/debian:packageBin
    ;;

"docker_build_and_push")
    
    cd ci/docker/
    ./run.sh
    ;;

"rholang_more_tests")

    # Prep nix 
    nix-env -iA nixpkgs.openjdk8 nixpkgs.sbt
    nix-env -f nix/default.nix -iA rchainPackages

    # Build rosette binary
    cd rosette
    nix-build

    cd ${TRAVIS_BUILD_DIR}

    ci/rholang-more-tests-main.sh
    ;;

*)

    echo "$SUBPROJECT build instructions are missing"
    exit 1

esac
