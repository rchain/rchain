#!/bin/bash

set -o nounset
set -o errexit


delete_resources_dir () {
    [ -d "$TEMP_RESOURCES_DIR" ] && rm -rf "$TEMP_RESOURCES_DIR"
}


main () {
    apt-get update
    env DEBIAN_FRONTEND=noninteractive apt-get -yq install make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl git

	if ! which pyenv &>/dev/null; then
		curl -fSL https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash
		export PATH="$HOME/.pyenv/bin:$HOME/.pyenv/shims:$PATH"
	fi

	cd integration-tests

	tag=DRONE-$DRONE_BUILD_NUMBER
	export DEFAULT_IMAGE=rchain-integration-tests:$tag
	sed "s/rnode:latest/rnode:$tag/" Dockerfile |\
		docker build --quiet --tag "$DEFAULT_IMAGE" --file - .

    trap delete_resources_dir EXIT
    TEMP_RESOURCES_DIR=$(mktemp --directory --tmpdir=/tmp integration-tests.${DRONE_BUILD_NUMBER:-0}.XXXXXXXXXX)
    cp -r resources/* "$TEMP_RESOURCES_DIR/"

	export TEMP_RESOURCES_DIR
    ./run_tests --log-cli-level=ERROR
}


main "$@"
