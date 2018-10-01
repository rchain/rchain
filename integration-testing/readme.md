# Installing the prerequisites

## Step 1. Install Docker
Follow the instructions on [this page](https://docs.docker.com/install/) for platform specific instalation steps

## Step 2. Python v3.6+
The integration tests run on **Python v3**, which is incompatible with Python v2. 
  
You can test the version of your default python installed on your machine:
```bash
$ python3 --version
``` 

If your version is lower than 3.6 you will have to either install or upgrade your Python 3 version.

### Option 1: Use a package manager
This is the simplest way to do it.

See below (a bit outdated) instructions for a few package managers. 
If you use something else, don't worry, Python 3 should be available for all major package managers. 

See below examples for a few platforms: 
[Debian Linux](https://docs.python-guide.org/starting/install3/linux/), 
[MacOS](https://docs.python-guide.org/starting/install3/osx/), 
[Windows](https://docs.python-guide.org/starting/install3/win/#install3-windows)

## Option 2: Download binaries
Go to https://www.python.org/downloads/ and download the available libraries

## Step 3: Dependencies
Once Python is installed you can run in this directory the following command: 
```bash
$ ./install_dependencies.sh
```

All the dependencies will be installed in the directory `.virtualenv` created in the current directory

You can test the installation like this:
```bash
$ .virtualenv/bin/python --version
Python 3.7.0
```
```bash
$ .virtualenv/bin/python -m pip show pytest docker
Name: pytest
Version: 3.7.1
Summary: pytest: simple powerful testing with Python
...
---
Name: docker
Version: 3.5.0
Summary: A Python library for the Docker Engine API.
...
``` 

 ## Step 4: Create the rnode docker image
 The tests use the docker image `coop.rchain/rnode:latest` as a base image for the docker containers. Docker has to be 
 able to get this image. 
 
 When the tests are run against the current source code one should build the docker image and publish it locally. For 
 details see [the developer information](https://github.com/rchain/rchain/blob/dev/DEVELOPER.md) 
 

# Running the tests

## Configuration
The file `pytest.ini` allows some configuration of the test execution. Information about all the available options 
can be found in [pytest.ini reference](https://docs.pytest.org/en/latest/reference.html#ini-options-ref)

## Execution

The tests are run using *pytest*. If you want to have a deep understanding of the whole framework you should check 
[pytest documentation](https://docs.pytest.org/en/latest/contents.html#toc) 

The tests can be run using the bash script 
```bash
$ ./run_tests.sh
```

In order to run only specific tests can specify the test subdir where you want the discovery to start

Examples:
Run the tests for the complete connected network: 
```bash
$ ./run_tests.sh test/test_complete_connected.py
```

You can see all the options available by running
```bash
$ ./run_tests.sh --help
```

To stop after the first failing tests or after N failure you can use `-x` or `--maxfail`
```bash
$ ./run_tests.sh -x
```
```bash
$ ./run_tests.sh --maxfail=3
```

The test discovery starts in the directories specified in the command line. 
If no directory is provided all the tests are run.

If you want to see what tests will be run by a certain command use the parameter `--collect-only`

Examples
```bash
$ ./run_tests.sh --collect-only
```
```bash
$ ./run_tests.sh --collect-only  test/test_star_connected.py
```


# Writing your own tests
## Pytest basics
Bellow is a basic introduction while [here](https://docs.pytest.org/en/latest/) you can see the full pytest documentation.

### Tests
Pytest tests are python functions which take fixtures as parameters.

Example:
```python
def test_one_plus_one():
    assert 1+1==2, "1+1 should equal 2"
    
def test_with_resources(some_resource):
    #...use the fixture some_resource

```

A successful test is a test which doesn't throw any exception. The `assert` statement can be used or `pytest.fail` or
simply an exception can be thrown
 
Note that the second test depends on some_resource. The parameter name has to have a corresponding fixture defined either 
in the test file or in one of the `conftest.py` visible by the current test.

### Fixtures
The fixtures define the setup/teardown logic. They are coroutines that look like this:

```python
@fixture(scope="package")
def some_resource(other_resource):
    #allocate the resource r
    yield r
    #cleanup the resource r

```

The fixture can depend on other fixtures by listing them in the parameter list the same way as the test functions to.

Fixtures have a scope which can be either `function`, `module`, `package` or `session`. If not specified, the default 
scope is `function`.

Fixtures are setup/teardown when the test execution enters/leaves its scope. For example `session` fixture will be setup only once 
at the beginning of the test execution while a `function` scoped fixture will be setup/teardown once for each test function
that depends on it.

## Test writing

### Test organization
The tests are organized in a tree-like structure under the directory `test`. Some subdirectories contain a file named 
`conftest.py` which contains the definitions of the fixtures that are available in that subdirectory.

Fixtures defined in a test subdirectory describe how the test contexts are setup/teardown. The tree structure of 
the tests is organized by fixtures. 

For example under `complete_connected` one can find tests that run on a complete connected network.

#### System fixture
This fixture is a *session* fixture, which means that it's created only once per test session. It is defined in `test/conftes.py`
and contains the elements needed for most tests:

1. config - the command line parameters
2. docker - an instance of a docker client
3. validator_data - contains the path to a tmp bonds file and the corresponding keys from resources/pregenerated-validator-private-public-key-pairs.txt

When this fixture is destroyed it:
1. removes all the docker unused networks and volumes 
2. removes the validator_data file
logs the profiling information for all the fucntions decorated with @profile


#### Package fixtures
The logic for setting up rnode networks are defined in `tools/network.py` as context managers. These are 
used in`conftest.py` and wrapped in a `@pytest.fixture` in order to build a network with the given shape.

This way the setup/teardown logic reused in several places.

### Waiting for conditions
Because simple `sleep`s are unreliable *all* waiting for various conditions is done via calls to wait-for.

The wait utilities are defined in `src/tools/wait.py`.

The key function is `wait_for` which waits for a given condition a certain number of seconds. This function checks 
periodically the condition to see if it is fullfilled.

There are also a predicates which can be used to define various conditions. One can write custom predicates
based on these examples. 

Please note that the predicates should have a readable `__doc__` attribute. 
They `__doc__` is important for debugging the tests because it's printed in the log files during the waiting. 

### File resources
The resources like contracts to be deployed, certificates etc. are stored in the `resources` directory. The code that 
needs access to these resources can access them using the utilities found in  `src/tools/resources.py`

### RNode interface
The file `src/tools/rnode.py` contains utilities for working with node.

### Profiling tests
The file `src/tools/profiling.py` contains the `profile` decorator which can be used to collect profiling information.
The profiling information is printed in the log file at the end of the test execution.

### Mixing fixtures
The file `src/tools/fixture.py` contains tools for parameterizing tests with different fixtures.

```python
@parametrize.cartesian(x=[a, b, c], y=[q,w,e])
def test_network_convergence(x, y):
    #...
```  

Based on this test there will be 9 tests generated, each of them called with one of the elements of the cartesian product
of the two fixture sets.


A similar result can be achieved by creating the 9 tests separately:
```python
def test_network_convergence_a_q(a, q):
    #...
def test_network_convergence_a_w(a, w):
    #...
def test_network_convergence_a_e(a, e):
    #...
def test_network_convergence_b_q(b , q):
    #...
...
```

The difference between the two approaches is the fact that wih `@parametrize.cartesian`, if the fixtures are session, 
package or module scoped, they are setup at the same time. In the above example if a,b,c,q,w,e will all live at the same 
time and might make the tests slow or fail if they use a lot of resources. This is the reason why test_casper_propose_and_deploy 
is not using `@paremetrize.cartesian`.

The second approach is more verbose but you have a finer control over the lifetime of the fixtures and you can control 
the resource usage better.

