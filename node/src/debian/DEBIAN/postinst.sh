#! /usr/bin/env bash

set -e

USERNAME="rnode"
DIRECTORY="/var/lib/${USERNAME}"

if id -u ${USERNAME} >/dev/null 2>&1; then
    echo "User ${USERNAME} already exists."
else
    adduser --no-create-home --home /nonexistent --group --system ${USERNAME}
fi

if [ -d ${DIRECTORY} ] ; then
    echo "Directory ${DIRECTORY} already exists."
else
    mkdir -p /var/lib/${USERNAME}/
    chown -R ${USERNAME}:${USERNAME} /var/lib/${USERNAME}/
fi

systemctl enable rnode.service