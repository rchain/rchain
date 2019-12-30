# Installing the prerequisites
## Step 1. Install Docker

If you don't know better, refer to the platform-specific installation
instructions at [Docker website](https://docs.docker.com/install/).

## Step 2. Install pyenv

`pyenv` allows for installing a standalone, isolated, specific CPython
version, making integration tests work irrespective of what CPython version
you have installed in your operating system.

### [Linux](https://github.com/pyenv/pyenv-installer#prerequisites)
### macOS

```bash
$ brew update && brew install pyenv
```

## Step 3. Install CPython under pyenv

```bash
$ pyenv install 3.7.3
```

## Step 4: Install pipenv under pyenv

```bash
$ env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pip install pipenv
```

## Step 4: Dependencies

```bash
$ cd integration-tests/
$ env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pipenv sync
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

## Running from Docker

TL;DR: If you want to run these tests from a Docker container, start the
container with `-v /var/run/docker.sock:/var/run/docker.sock -v /tmp:/tmp`.

These tests can be run from a Docker container, but in that case a) the Docker
socket has to be accessible (writeable) by the container, and b) the temporary
directory **in the container** (either `/tmp` or whatever is in the environment
variable `$TMPDIR`) has to be accessible by the host as well **on the same
path**. The reason for the latter is that tests spawn additional containers and
need to share files with them. They do it by mounting files/directories from
`/tmp` (or `$TMPDIR`) into new containers, i.e. by starting new containers with
e.g. `-v /tmp/bonds.txt:/var/lib/rnode/genesis/bonds.txt` arugments. But these
arguments are passed via shared Docker socket to Docker daemon running on the
host. So the Docker daemon has to be able to access `/tmp/bonds.txt` as well.

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

## Troubleshooting

If you're on macOS and getting exceptions similar to

```
self = <Response [403]>

    def raise_for_status(self):
        """Raises stored :class:`HTTPError`, if one occurred."""

        http_error_msg = ''
        if isinstance(self.reason, bytes):
            # We attempt to decode utf-8 first because some servers
            # choose to localize their reason strings. If the string
            # isn't utf-8, we fall back to iso-8859-1 for all other
            # encodings. (See PR #3538)
            try:
                reason = self.reason.decode('utf-8')
            except UnicodeDecodeError:
                reason = self.reason.decode('iso-8859-1')
        else:
            reason = self.reason

        if 400 <= self.status_code < 500:
            http_error_msg = u'%s Client Error: %s for url: %s' % (self.status_code, reason, self.url)

        elif 500 <= self.status_code < 600:
            http_error_msg = u'%s Server Error: %s for url: %s' % (self.status_code, reason, self.url)

        if http_error_msg:
>           raise HTTPError(http_error_msg, response=self)
E           requests.exceptions.HTTPError: 403 Client Error: Forbidden for url: http+docker://localhost/v1.35/networks/4c5079902ad7d50a0c7a763ac6a022923c2fe2e4ceb608952c67d433b428e891
```

make sure you have at least 4GiB of RAM set for use by the Docker Engine
(macOS system menu bar -> Docker icon -> Preferences... -> Advanced).
