#!/bin/bash -ue

MYPYPATH=test pipenv run py.test --mypy -m mypy "$@"
