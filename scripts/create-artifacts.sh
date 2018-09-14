#! /usr/bin/env bash

set -eo pipefail

if [ "$SUBPROJECT" = "core" -a -n "$TRAVIS_TAG" ]
then
    sbt -Dsbt.log.noformat=true clean \
        rholang/bnfc:generate \
        node/debian:packageBin \
        node/rpm:packageBin \
        node/universal:packageZipTarball
elif [ "$SUBPROJECT" = core-java10 -a -n "$TRAVIS_TAG" ]
then
    sbt -Dsbt.log.noformat=true clean \
        rholang/bnfc:generate \
        node/debian:packageBin \
        node/rpm:packageBin \
        node/universal:packageZipTarball

    # Rename produced artifacts to avoid conflicts with other Java versions in the Travis' build matrix:

    deb_package=$(ls node/target/*.deb)
    mv "$deb_package" node/target/java10-$(basename "$deb_package")

    rpm_package=$(ls node/target/rpm/RPMS/noarch/*.rpm)
    mv "$rpm_package" node/target/rpm/RPMS/noarch/java10-$(basename "$rpm_package")

    tarball=$(ls node/target/universal/*.tgz)
    mv "$tarball" node/target/universal/java10-$(basename "$tarball")
fi
