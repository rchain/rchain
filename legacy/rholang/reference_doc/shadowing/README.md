## Shadowing

### About Shadowing
Like many languages, rholang allows more closely bound names to shadow more loosely bound names. In this example, the receive matched both sends because `y` is used as a binder and matches any process.

```rholang
new x, y, commChan, stdout(`rho:io:stdout`) in {

  commChan!(x!(*y)) |
  commChan!(x!(4)) |

  for (@{x!(y)} <= commChan){
    stdout!(y)
  }
}
```

### Avoiding Shadowing
In this example, only the first send matches because the pattern is not using `y` as _any_ variable and a binder. Rather it is requiring that `y` match the exact same name defined on line 1.

```rholang
new x, y, commChan, stdout(`rho:io:stdout`) in {

  commChan!(x!(*y)) |
  commChan!(x!(4)) |

  for (@{x!(=*y)} <= commChan){
    stdout!(*y)
  }
}
```

Another example from Mike Stay
```rholang
for (@x <- chan1) {
  for (@=x, @y <- chan2) {
    stdout!(y)
  }
}
```
