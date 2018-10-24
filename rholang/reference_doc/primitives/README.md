## Primitive data types

Rholang has several primitive data types built in.

### Integers
Rholang's integers are 64-bit signed, represented internally as two's compliment binary data. These integers are "unsafe" in the sense that they make no attempt to avoid over- or underflow. There are no built-in fixed- or floating-point numbers.

| Integer Max Value |  9223372036854775807 |
| Integer Min Value | -9223372036854775808 |

#### Arithmetic
Integers support basic arithmetic operations natively, and other operations are being written in the [standard library](https://github.com/JoshOrndorff/librho/UnsafeMath)

```rholang
new stdout(`rho:io:stdout`) in {
  // Addition
  stdout!(5 + 4) |

  // Subtraction
  stdout!(-3 - 4) |

  // Multiplication
  stdout!(-20 * -30) |

  // Integer Division
  stdout!(-90 / 6) |

  // Watchout for overflow
  stdout!(9223372036854775800 + 10) |

  // Watch out for underflow
  stdout!(-9223372036854775800 - 10)
}
```

#### Comparison
Integers also support common binary operators for comparison which evaluate to Boolean primitives. The supported operators are `<`, `<=`, `==`, `!=`, `>=`, `>`.

```rholang
new stdout(`rho:io:stdout`) in {

  stdout!(5 < 4) |

  stdout!(5 >= 4) |

  stdout!(100 != 200)
}
```

### Booleans
Boolean literals are spelled `true` and `false`. Rholang natively supports binary operators on Booleans.

```rholang
new stdout(`rho:io:stdout`) in {

  stdout!(true and true) |

  stdout!(true or false) |

  stdout!(not false)
}
```

### URIs

### Strings?
Strings are covered in detail under [data structures](../data_structures). Here are a few basics

```rholang
new stdout(`rho:io:stdout`) in {
  stdout!("String" ++ "Concatenation")|
}
```
