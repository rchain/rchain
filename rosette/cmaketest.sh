#!/usr/bin/env bash



# Create file for printing results
touch rbl/rosette/results.txt

# Build Rosette
./clean.sh
./build.sh

# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
./build.out/src/rosette -boot rbl/rosette/boot.rbl \
    -run rbl/rosette/hello_world.rbl <<EOF
(exit)
EOF

./build.out/src/rosette -boot rbl/rosette/boot.rbl <<EOF
(seq
  (display "\n")		; (terpri)
  (load "tests/equiv.ros" 'silent))
EOF

# Check if any tests have failed
cat rbl/rosette/results.txt | grep "Fail"
if [ $? -ne 1 ]
    then
    exit 1
fi

# Otherwise, return success
exit 0
