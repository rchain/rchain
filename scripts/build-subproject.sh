#!/bin/bash

echo
echo ----
echo

#check for bash test script
if [ -f "$SUBPROJECT/test.sh" ]
then
    echo "$SUBPROJECT/test.sh"
    (cd $SUBPROJECT; bash test.sh)

#check for sbt subproject
elif [ -f "$SUBPROJECT/build.sbt" ]
then
    echo "$SUBPROJECT/build.sbt"
    (cd $SUBPROJECT; sbt -Dsbt.log.noformat=true test)
fi
