#!/bin/bash

if [ -f "$SUBPROJECT/install.sh" ]
then
    (cd $SUBDIR; ./install.sh)
fi
