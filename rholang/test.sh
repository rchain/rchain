sbt -Dsbt.log.noformat=true bnfc:generate
sbt -Dsbt.log.noformat=true assembly
# Run compiler over all test rholang files

run_tests ()
{
    for i in tests/*; do
      echo $i
      ./rho2rbl "$i"
    done
    # Rewrite all tests to print to a text file results.txt instead of stdout
    sed -i 's/print/ostream-print (ostream-new "results.txt")/g' tests/*.rbl
    # Move all generated tests under the rosette/tests folder to run
    mv tests/*.rbl ../rosette/rbl/rosette/tests/
    for i in failure_tests/*.rho; do
      if ./rho2rbl "$i" ; then
        rm "${i%.*}.rbl"
        exit 1
      fi
    done
}

if run_tests
then
    # Upload test coverage reports to CodeCov
    bash <(curl -s https://codecov.io/bash) -c -F rholang 
    exit 0
else
    # On failure, exit with exit status of last command (run_tests)
    exit $?
fi
