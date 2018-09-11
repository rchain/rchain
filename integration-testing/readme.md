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
$ ./run_tests.sh --collect-only  test/star_connected
```

# Writing your own tests
TODO