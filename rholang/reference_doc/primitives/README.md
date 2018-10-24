## Primitive data types

Rholang has several primitive data types built in.

### Integers
Rholang's integers are 64-bit signed, represented internally as two's compliment binary data. These integers are "unsafe" in the sense that they make no attempt to avoid over- or underflow. There are no built-in fixed- or floating-point numbers.

| Integer Max Value |  9223372036854775807 |
| Integer Min Value | -9223372036854775808 |

#### Arithmetic
Integers support basic arithmetic operations natively, and others are being written in the [standard library](https://github.com/JoshOrndorff/librho/UnsafeMath)

```rholang
new stdout(`rho:io:stdout`) in {
  stdout!(5 + 4) |
  // show all including overflow and underflow
}
```

#### Comparison


### Booleans
