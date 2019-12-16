#!/bin/bash

set -o nounset
set -o errexit


delete_resources_dir () {
    [ -d "$TEMP_RESOURCES_DIR" ] && rm -rf "$TEMP_RESOURCES_DIR"
}


main () {
    apt-get update
    env DEBIAN_FRONTEND=noninteractive apt-get -yq install make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl git

    curl https://pyenv.run | bash
    ~/.pyenv/bin/pyenv install 3.7.3
    env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pip install pipenv

    cd integration-tests
    env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pipenv sync

    trap delete_resources_dir EXIT
    TEMP_RESOURCES_DIR=$(mktemp --directory --tmpdir=/tmp integration-tests.${DRONE_BUILD_NUMBER:-0}.XXXXXXXXXX)
    cp -r resources/* "$TEMP_RESOURCES_DIR/"

    ./mypy.sh
    ./pylint.sh
	export TMPDIR=$TEMP_RESOURCES_DIR
    ./run_tests.sh --log-cli-level=ERROR
}


main "$@"
