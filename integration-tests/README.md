# Installing the prerequisites

## Step 1. Install Docker

Follow the instructions on [this page](https://docs.docker.com/install/) for platform specific instalation steps

## Step 2. Python v3.6+

The integration tests run on **Python v3**, which is incompatible with Python v2. 
  
You can test the version of your default python installed on your machine:

```bash
$ python3 --version
``` 

If your version is lower than 3.6 you will need to install Python 3.

### macOS

macOS is shipped with Python 2.x, so we install Python 3.x from Homebrew:

```bash
$ brew update && brew install python
$ python3 --version
Python 3.7.0
```

### [Debian Linux](https://docs.python-guide.org/starting/install3/linux/)
### [Windows](https://docs.python-guide.org/starting/install3/win/#install3-windows)

## Step 3: [Install pipenv](https://github.com/pypa/pipenv#installation)

## Step 4: Dependencies


Once Python is installed you can run within `integration-tests` subdirectory
the following command:

```bash
$ pipenv sync
```


## Step 4: Create the rnode docker image

Tests use RNode Docker image. If environment variable `${DRONE_BUILD_NUMBER}` is
defined, then `coop.rchain/rnode:DRONE-${DRONE_BUILD_NUMBER}` image is used.
These are created on Drone CI in order to use have image per build. If the
variable is undefined, `coop.rchain/rnode:latest` is used.
 
When the tests are run against the current source code one should build the
docker image and publish it locally. For details see [the developer
information](https://github.com/rchain/rchain/blob/dev/DEVELOPER.md) 
 
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

In order to run only specific tests can specify the test subdir where you want
the discovery to start

Examples:
Run the tests for the complete connected network: 

```bash
$ ./run_tests.sh test/test_complete_connected.py
```

You can see all the options available by running

```bash
$ ./run_tests.sh --help
```

To stop after the first failing tests or after N failure you can use `-x` or
`--maxfail`:

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

The test can runs the [mypy](https://pypi.org/project/pytest-mypy/) static type checker on your source files as part of 
your Pytest test runs now. It is not enabled by default now. You can run the static type checker test by the command below.

```bash
$ ./run_tests.sh --mypy
```

If you want to restrict your test run to only perform mypy checks and not any other tests by using the `-m` option.

```bash
$ ./run_tests.sh --mypy -m mypy 
```
