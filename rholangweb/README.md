# RholangWeb

This is a web interface to the Rholang compiler, inspired by
[the crystal playground][cr].

[cr]: https://play.crystal-lang.org/#/cr


## Security Considerations / Warnings

Code from each HTTP request is given to the compiler via a temporary
directory created in a reasonably secure manner (using `tmpfile` from
the python standard library).

We assume the compiler writes to foo.rbl when given foo.rho and is
otherwise well-behaved.

Settings are used to limit

  - compiler: runtime
  - VM: stacksize and runtime

**WARNING**: the VM subprocesses is not otherwise constrained.


## Getting Started

This is a typical [django][] app.

Be sure you have the rholang compiler jar and the rosette executable
and `rbl` library; adjust `rholang/settings.py` to say where they are.

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
