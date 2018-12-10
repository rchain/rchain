#!/bin/bash -ue

main () {
    pushd "$(dirname $0)" >/dev/null
    MYPYPATH=test pipenv run py.test --mypy -m mypy "$@"
}

main "$@"
