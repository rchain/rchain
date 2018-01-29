#!/bin/bash

echo
echo ----
echo

# Check for bash build script.
if [ -f "${SUBPROJECT}/build.sh" ] ; then {
    echo "${SUBPROJECT}/build.sh"
    (cd ${SUBPROJECT}; bash build.sh)
};
#check for bash test script
elif [ -f "$SUBPROJECT/test.sh" ] ; then {
    echo "$SUBPROJECT/test.sh"
    (cd $SUBPROJECT; bash test.sh)

};
#check for sbt subproject
elif [ -f "$SUBPROJECT/build.sbt" ] ; then {
    echo "$SUBPROJECT/build.sbt"
    (cd $SUBPROJECT; sbt -Dsbt.log.noformat=true test)
}; else {
    echo "${SUBPROJECT} - No build/test files found."
    exit 1;
}; fi
