# Building docker images

The facilities in this directory merely bundle various RChain subprojects into docker images so that they may be run in containers. One advantage of this is that run-time dependencies do not have to be installed on every machine running this software. Another is that it makes perfectly clear what the runtime dependencies are.

## Preliminaries

The first step is to ensure that the RChain subprojects at `rchain/rholang` and `rchain/rosette` build independent of the docker process. Each subproject includes build instructions, and that's a great place to start.

## Building images and running containers

There are two main `make` targets:

### rholang-cli
`make rholang-cli` constructs a docker image that can compile and execute a Rholang contract. It bundles together the Rholang-to-Rosette compiler in `rchain/rholang` and the Rosette interpreter in `rchain/rosette`. The resulting image is tagged `rholang-cli`.

Once built, you could run it from this directory like this:

```
$ docker run -v $PWD/../rholang/examples/hello_world_again.rho:/tmp/input.rho rholang-cli
compiled /tmp/input.rho to /tmp/input.rbl
*** warning: more than one result may be returned from a block expression
Rosette System, Version 4.0.0 - Copyright 1989, 1990, 1991, 1992 MCC
 - Copyright 2017, 2018 RChain Cooperative
"Hello World"
"Hello World again"
[]
```

The filename is passed into the container with the `-v` (volume) command. The full path (absolute) must be specified, and it will be mapped to `/tmp/input.rho` in the container, which is where the script that runs the Rholang compiler expects its input. The output of that is then passed to Rosette, which executes it. There is a shell script included that does this thing (`docker/rholang-cli/rhoscala`), but I'm not sure how to distribute that.

### rholang-web
`make rholang-web` builds Dan Connolly's (@dckc) web application into a docker image. It also bundles the Rholang compiler and the Rosette interpreter. This process yields an images tagged `rholang-web`.

Run the thing like
```
$ docker run -ti --net host rholang-web
Performing system checks...

System check identified no issues (0 silenced).
02:58 DEBUG (0.001)
            SELECT name, type FROM sqlite_master
            WHERE type in ('table', 'view') AND NOT name='sqlite_sequence'
            ORDER BY name; args=None
02:58 DEBUG (0.000) SELECT "django_migrations"."app", "django_migrations"."name" FROM "django_migrations"; args=()
January 19, 2018 - 02:58:24
Django version 1.11, using settings 'rholang.settings'
Starting development server at http://127.0.0.1:8000/
Quit the server with CONTROL-C.
```
and you'll be able to point a browser at [http://localhost:8000](http://localhost:8000).

