# Modify configuration.rbl to run tests
for i in rbl/rosette/tests/*; do
	filename=$(basename "$i")
	filename_without_extension="${filename%.*}"
	echo "(load 'tests/$filename_without_extension 'silent)" >> rbl/rosette/configuration.rbl
done
echo "(exit)" >> rbl/rosette/configuration.rbl

# Create file for printing results
touch rbl/rosette/results.txt

# Build Rosette
make

# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
./bin/boot-ess -boot rbl/rosette/boot.rbl

# Check if any tests have failed
cat rbl/rosette/results.txt | grep "Fail"
if [ $? -ne 1 ]
    then
    exit 1
fi

# Otherwise, return success
exit 0
