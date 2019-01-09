#!/bin/bash -ue

main () {
    pushd "$(dirname $0)" >/dev/null
    pipenv run mypy "$@" test
}

main "$@"
