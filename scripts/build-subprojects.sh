#!/usr/bin/env bash
set -e

case "$SUBPROJECT" in "rosette")

    cd rosette
    nix-build

    ./run.sh rbl/rosette/tests/simple_add.rbl
    ;;

"core" | core-java10)

    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate casper/test:compile coverage scalafmtCheck test rspace/it:test coverageReport rspace:tut

    curl -s https://codecov.io/bash >/tmp/codecov
    chmod +x /tmp/codecov

    for sub in block-storage casper crypto comm rholang roscala node rspace shared
    do
        /tmp/codecov -X gcov -s ./$sub -c -F ${sub//-}
    done
    ;;

"test_artifact_creation")

    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate \
        node/rpm:packageBin \
        node/debian:packageBin \
        node/universal:packageZipTarball
    ;;

"rnode-dockerhub-push")

    ./scripts/rnode-dockerhub-push.sh
    ;;

"p2p-test-network")

    sudo apt-get -yq install python3-minimal python3-pip
    sudo pip3 install pexpect argparse docker
    ./scripts/p2p-test-network.sh
    ;;

"cloud-p2p-test-network")

    ./scripts/cloud-p2p-test-network.sh
    ;;

"rholang_more_tests")

    # Prep nix
    nix-env -iA nixpkgs.openjdk8 nixpkgs.sbt
    nix-env -f nix/default.nix -iA rchainPackages

    # Build rosette binary
    cd rosette
    nix-build

    cd ${TRAVIS_BUILD_DIR}

    scripts/rholang-more-tests-main.sh
    ;;

*)

    echo "$SUBPROJECT build instructions are missing"
    exit 1

esac
