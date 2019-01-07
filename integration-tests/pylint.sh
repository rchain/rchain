#!/bin/bash -ue

main () {
    pushd "$(dirname $0)" >/dev/null
    pipenv run py.test --pylint -m pylint "$@"
}

main "$@"
