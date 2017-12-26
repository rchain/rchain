# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
./bin/boot-ess -boot rbl/rosette/boot.rbl -run ../rholang/examples/hello_world_again.rbl
