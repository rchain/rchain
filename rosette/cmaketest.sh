#!/usr/bin/env bash

set -e

# Create/clear file for printing results
RESULTS=rbl/rosette/results.txt
cat > "${RESULTS}" < /dev/null

# Build Rosette
./clean.sh
./build.sh

{
    # Continue after errors
    set +e

    # Increase stack limit so that Rosette can load all files
    ulimit -s unlimited

    export ESS_SYSDIR=$PWD/rbl/rosette
    ROSETTE="./build.out/src/rosette --boot-dir rbl/rosette"

    # Execute Rosette instance
    ${ROSETTE} rbl/rosette/hello_world.rbl <<EOF
(exit)
EOF

    ${ROSETTE} <<EOF
(seq
  (display "\n")		; (terpri)
  (load "tests/run-tests.ros" 'silent))
EOF

    # Segfault regression test hacks

    for f in rbl/rosette/tests/segfaults/*.rbl
    do
	${ROSETTE} < $f
	res=$?
	if [ ${res} -ne 0 ]
	then
	    echo -e "\033[31mRegression failure (exit code ${res}) on $f\033[39m"
	else
	    echo -e "\033[32mRegression success (exit code ${res}) on $f\033[39m"
	fi
    done
} 2>&1 | tee -a "${RESULTS}"

echo
echo Summary:

# Check if any tests have failed
grep fail "${RESULTS}"
if [ $? -eq 0 ]
then
    exit 1
fi

echo All good.
exit 0
