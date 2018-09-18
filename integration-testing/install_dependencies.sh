#!/usr/bin/env bash

python3 -m venv ./.virtualenv

.virtualenv/bin/pip install --upgrade pip
.virtualenv/bin/pip install docker pytest delayed_assert