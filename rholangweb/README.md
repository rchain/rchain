# RholangWeb

This is a web interface to the Rholang compiler, inspired by
[the crystal playground][cr].

[cr]: https://play.crystal-lang.org/#/cr
[RHOL-35]: https://rchain.atlassian.net/projects/RHOL/issues/RHOL-35


## Getting Started

This is a typical [django][] app, so once you clone it:

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


## Development

For context, see [RHOL-35][].

## Troubleshooting

If `rosette` dumps core, try relaxing the maximum stack size with
`ulimit -s unlimited`.
