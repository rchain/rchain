#/bin/bash

set -o nounset
set -o errexit


delete_resources_dir () {
    [ -d "$TEMP_RESOURCES_DIR" ] && rm -rf "$TEMP_RESOURCES_DIR"
}


main () {
    python3 -m pip install pipenv

    cd integration-tests
    pipenv sync

    trap delete_resources_dir EXIT
    TEMP_RESOURCES_DIR=$(mktemp --directory --tmpdir=/tmp integration-tests.${DRONE_BUILD_NUMBER:-0}.XXXXXXXXXX)
    cp -r resources/* "$TEMP_RESOURCES_DIR/"

    ./mypy.sh
    ./pylint.sh
    ./run_tests.sh --log-cli-level=ERROR --mount-dir="$TEMP_RESOURCES_DIR" -n 2 --dist=loadscope
}


main "$@"
