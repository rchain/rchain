# RholangWeb

This is a web interface to the Rholang compiler, inspired by
[the crystal playground][cr].

[cr]: https://play.crystal-lang.org/#/cr


## Security Considerations / Warnings

**WARNING: Do not expose this web service to untrusted parties. It is
not yet suitable for other than local use.** Submitted code is run in
a relatively unconstrained rosette VM process that can, for example,
open and write to local files using `ostream-new`.

Code from each HTTP request is given to the compiler via a temporary
directory created in a reasonably secure manner (using `tmpfile` from
the python standard library).

We assume the compiler writes to foo.rbl when given foo.rho and is
otherwise well-behaved.

Settings are used to limit

  - compiler: runtime
  - VM: stacksize and runtime


## Getting Started

This is a typical [django][] app.

Be sure you have the rholang compiler jar and the rosette executable
and `rbl` library; adjust `rholang/settings.py` to say where they are. Alternatively, set the following environment variables

 * `RHOLANGWEB_EXAMPLES_DIR` The location of the `examples/` directory of sample Rholang contracts.
 * `RHOLANGWEB_COMPILER_JAR` The path to the rholang compiler's JAR file.
 * `RHOLANGWEB_VM_PROGRAM` The path to the `rosette` binary.
 * `RHOLANGWEB_VM_LIBRARY` The location of the Rosette bootstrap files.

Then:

```
RholangWeb$ pip install -r requirements.txt 
...
Successfully installed Django-1.11 django-extensions-1.6.1 django-widget-tweaks-1.3 pytz-2017.3 six-1.11.0

RholangWeb$ python manage.py migrate
Operations to perform:
  Apply all migrations: admin, auth, contenttypes, sessions
...

RholangWeb$ python manage.py runserver
Performing system checks...

System check identified no issues (0 silenced).
January 06, 2018 - 15:36:29
Django version 1.11, using settings 'rholang.settings'
Starting development server at http://127.0.0.1:8000/
```

[django]: https://www.djangoproject.com/


### To Do

  - log programs, results
    - privacy policy

## Development

For context, see [RHOL-35][].

[RHOL-35]: https://rchain.atlassian.net/projects/RHOL/issues/RHOL-35
