#!/bin/bash

set -o nounset
set -o errexit


delete_resources_dir () {
    [ -d "$TEMP_RESOURCES_DIR" ] && rm -rf "$TEMP_RESOURCES_DIR"
}


main () {
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
    ./run_tests.sh --log-cli-level=ERROR --mount-dir="$TEMP_RESOURCES_DIR"
}


main "$@"
