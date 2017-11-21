sbt -Dsbt.log.noformat=true bnfc:generate
sbt -Dsbt.log.noformat=true assembly
# Run compiler over all test rholang files
for i in tests/*; do
	./rho2rbl "$i"
done
# Rewrite all tests to print to a text file results.txt instead of stdout
sed -i 's/print/ostream-print (ostream-new "results.txt")/g' tests/*.rbl
# Move all generated tests under the rosette/tests folder to run
mv tests/*.rbl ../rosette/rbl/rosette/tests/
