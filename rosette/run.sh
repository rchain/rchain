# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
export ESS_SYSDIR=rbl/rosette
./build.out/src/rosette --boot-dir rbl/rosette rbl/rosette/hello_world.rbl
