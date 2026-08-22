# A Tutorial for Matching in Rholang

## Patterns we are allowed to match

Let's clarify the implications of the following sentence from the Rholang tutorial: "There are two kinds of values in Rholang, names and processes. Patterns are names or processes with free variables, which appear to the left of an arrow in a `for` or after the `match` keyword."

The first is that a program cannot have any globally free variables. It also can't have any logical connectives, wildcards, or expressions using list or set remainders unless they form part of a pattern. For example, the following code snippets are not valid programs, despite the fact that they are valid components of patterns:

* `@Nil!(Nil) /\ @Nil!(Nil)` (logical connective)
* `for( x <- @"channel" ){ _ }` (wildcard)
* `[1 , 2 ... x]` (list remainder)

The second, in the same vein, is that a process variable does *not* match with anything that is not a process, meaning that it cannot match with a statement that contains a free variable, logical connectives, wildcards, or expressions using list or set remainders, unless those are written in a pattern fully contained in the statement.  Likewise, a name variable cannot match with anything that is not a quoted process, in the sense that it cannot contain free variables, logical connectives, etc unless they are correctly written in a pattern fully contained in the quoted process. For example, the following code

    1 match {for( x <- @Nil ){ Nil }} {
    2       {for( x <-   y  ){ Nil }} => { y!(Nil) }
    3 }

*will* match, returning `@Nil!(Nil)`, but

    1 match {for( x <-  @{for( x <- @Nil ){ x!(Nil) }} ){ Nil }} {
    2       {for( x <-  @{for( x <- @Nil ){    y    }} ){ Nil }} => { y }
    3 }

will not match since `y` cannot match with `x!(Nil)`, and thus evaluate to the empty process. Also,

    for( x <- @{z!(Nil)} ){ Nil }

won't compile, due to the globally free variable `z`,

    for( _ <- @{Nil} ){ {Nil} \/ {@Nil!(Nil)} }

is incorrect, due to a logical connective being where a process should be, and

    for( x <- @{[1, 2 ... z]}){ Nil }

won't compile because `z` is free and the given list is a pattern.

## Name Equivalence

The RHO calculus, on which Rholang is based, says that two names are equivalent when the processes that they quote are equivalent, and that processes are equated via the relations below (and nothing more). Here, `P`, `Q` and `R` are processes:

* `P | Q = Q | P` (commutativity)
* `P = P | Nil = Nil | P` (identity)
* `(P | Q) | R = P | (Q | R)` (associativity)

Therefore, `@ P | Q = @ Q | P` and `@ P | Nil = @ P = @ Nil | P`, etc.

In Rholang, these relations only apply *to the top level* of any name. In addition to the three given above, we also evaluate expressions on the top level. So for example, as channels,

* `@{10 + 2} = @{5 + 7}` and
* `@{for(x <- @Nil){ 10 + 2 }} = @{for(x <- @Nil){ 5 + 7 }}`.

Since we use variables, channels also respect alpha equivalence, meaning that, for example, as channels

* `@{for( x <- @Nil ){ Nil }}  = @{for( z <- @Nil ){ Nil }}`.

In the RHO calculus we don't have to worry about distinguishing the top level from other parts of a channel, but because of things like pattern-matching, we have to in Rholang. This will be relevant later on, where there are restrictions on pattern-matching because of this.

## "Looking through the looking glass": Patterns Within Patterns

Patterns follow the same rules as channels in terms of equivalence. In the case of -arity matching, joins, logical connectives, etc. the rules apply to each name pattern individually.

Here we need to treat statements that are not on the top level. We reemphasize that the rules for name equivalences described in the last section only apply on the top level. When we are matching *patterns within patterns*, these equivalence rules do not apply and we check for an exact text match, **excluding** alpha equivalence. This means that the following two channels are *not* equivalent:


* `@for(  @for( @{ Nil | x } <- @Nil ){ Nil } <- @Nil ){ Nil }}`
* `@for(  @for( @{ x | Nil } <- @Nil ){ Nil } <- @Nil ){ Nil }}`.

Note that while the patterns `@{ x | Nil }}` and `@{ Nil | x }` are equivalent at the top level, the patterns

* `@for( @{ Nil | x } <- @Nil ){ z }`
* `@for( @{ x | Nil } <- @Nil ){ z }`

are *not*. When we are matching *patterns within patterns*, these equivalence rules do not apply and we check for an exact text match, up to alpha equivalence. The principle is that you can only look through the looking glass once: when checking name equivalence or matching a pattern to a name or a process, if a given pattern is part of a larger pattern, there has to be an exact match.

Furthermore, we can't bind variables to parts of patterns. For example, the following send/receive will not match:

    1 for( @{for( @{x!(y)} <- @Nil ){ Nil }} <- @Nil ){ Nil }  |
    2 @Nil!( for( @{x!(10)} <- @Nil ){ Nil } )

Naively, one might expect these to match, binding `y` to `10`, but to match with the above receive, one must send something alpha equivalent to:

    @Nil!( for( @{x!(y)} <- @Nil ){ Nil } )

## Precedence Rules
When writing more complicated patterns, one should be aware of precedence rules of the operators in Rholang. Take for example the following pattern:

    @{@x!(Nil) | y!(Nil)}

A priori, it is unclear whether this should be interpreted as `@{{@x!(Nil)} | {y!(Nil)}}`, where `x` is a process variable and `y` is a name variable, or as `@{@{x!(Nil) | y}!(Nil)}`, where `x` is a name variable and `y` is a process variable. The chosen interpretation is vital, since the way we use `x` and `y` in the body depend on the type (process or name) of each of these terms. If the first interpretation is correct,

    for( @{@x!(Nil) | y!(Nil)} <- @10 ){ @x!("success") | y!("success") }

will compile, while

    for( @{@x!(Nil) | y!(Nil)} <- @10 ){ x!("success") | @y!("success") }

will not. If the second interpretation is correct, the opposite is true.

In Rholang, `@` binds tighter than `|`. Thus our first interpretation, `@{{@x!(Nil)} | {y!(Nil)}}`, is correct. We also have precedence rules for logical operations. Take for example the pattern

    x /\ @y!(Nil) \/ @z!(10)

Like in the example above, we could either interpret this as `{x /\ @y!(Nil)} \/ @z!(10)`, or as `x /\ {@y!(Nil) \/ @z!(10)}`. In Rholang, `/\` takes precedence over `\/` (as is the standard for logical connectives), so the first interpretation is correct.

## Illegal Moves
There are some illegal moves that we ought to cover. The first has to do with arithmetic operations which, if you recall from the section on name equivalence, are evaluated on the top level, but not anywhere else. Because of this, in Rholang we cannot match parts of arithmetic operations. For example, we might expect

    for( @{x + 7} <- @Nil ){ Nil }

to match with a send such as `@Nil!(5 + 7)`, binding `x` to `5`. However, when messages are sent, top-level expressions are evaluated before sending, so this send will not match with anything&mdash;although it will compile!

Remember, however, that an arithmetic operation that is in a pattern within a pattern is not evaluated when sent over a channel, and must be matched exactly. For example,

    @Nil!(for( @{5 + 7} <- @Nil ){ Nil })

can only match with something that preserves the `5 + 7` intact. This means that to match with this we need something of the form

    1 for(
    2   @{for( @{5 + 7} <- ... ){ ... }}   <-   @Nil )
    3   { ... }
    4 )

If we wanted to bind a variable to the `5`, for example, we would need something of the form

    1 for(
    2   @{for( @{x + 7} <- ... ){ ... }}   <-   @Nil )
    3   { ... }
    4 )

which would never match because patterns within patterns must match exactly.

However, when the arithmetic operation is both (1) not on the top level of a pattern, and (2) not part of a pattern within a pattern, we *can* bind to parts of an arithmetic expression. For example, the receive

    for( @{for( x <- @{5 + w}){Nil}} <- @Nil ){ @Nil!(w) }

can take a message from a send of the form

    @Nil!( for( x <- @{5 + 7}){Nil} )

evaluating to `@Nil!(7)`.

The other illegal move we ought to cover has to do with the `match` process. Remember that we cannot bind a free variable to any process or name containing free variables, or containing any out-of-context uses of logical connectives, joins, etc. In particular, it is syntactically incorrect to write:

    1 match {match {@Nil!(Nil)} { x => {@Nil!(x)} }}
    2   {
    3       {match {@Nil!(Nil)} { y }} => { y }
    4   }

since, if it did match, `y` would match to `x => {@Nil!(x)}`, which is neither a process nor a name.

## Patterns With Parallel Processes
When using parallel processes in patterns, it is sometimes not immediately obvious how a pattern will match. Take for example the following receive:

    for( @{x | y} <- @Nil ){ Nil }

Given a send of the form `@Nil!( 10 | 20 )`, we might expect the receive to bind `x` to `10` and `y` to `20`. However, this match is not so straightforward. Since `x | y = y | x`, we just as well could match `x` to `20` and `y` to `10`. Futhermore, since `10 | 20 = 10 | 20 | Nil = Nil | 10 | 20`, the match could take place in a number of ways.

This send/receive will nondeterministically either bind `x` to `10`, `x` to `10 | 20` or `x` to the empty process `Nil` while `y` binds to whatever `x` does not take, binding to `Nil` if `x` binds to `10 | 20`. This nondeterminism applies to wildcards, too. As the interpreter stands, Rholang will choose one of `x` and `y`, give it `10 | 20`, and bind the other variable to `Nil`. Regardless, it is incorrect to write a program with such a pattern that relies on any of the variables matching to a specific piece of a message.

Note further that since `P = P | Nil` for any process `P`, the pattern `@{x | y}` can match with a process with no parallel components. Likewise, `@Nil!(10 | 20)` would send successfully to something of the form `for( @{x | y | z} <- @Nil ){ Nil }`, where we've added an extra variable on the receiving pattern.

Note that we can write patterns with parallel processes that are not just process variables, such as `@{ @Nil!(x) | for( z <- c ){ z!(Nil) } }`, etc.

## Logical Connectives

Logical connectives can be incorporated into patterns, but we cannot send over a channel which contains logical connectives. For example, it is incorrect to write

    @{ {10} \/ {20} }!(Nil)

We can, however, correctly write

    for( @{@{ {10} \/ {20} }!(z)} <- @Nil ){ @Nil!(z) }

which listens on `@Nil` for a process which sends either to `@10` or to `@20`. Whatever is being sent is grabbed and sent over `@Nil` in the body. If we use `/\` in a pattern, any free variables which are in the pattern will be bound to their corresponding parts. However, if we use `\/`, none of the variables in the parse tree below the node are bound. In particular, if `P1 \/ P2` is in a pattern, the program will fail to run if any free variables in `P1` or `P2` appear in the body, since they cannot be guaranteed to bind to anything.

We note here that in Rholang only processes can be logically connected.

## Wildcards vs Variables
There is an important, somewhat subtle difference between wildcards and variables in Rholang. A wildcard can be placed anywhere that a *free* variable can be placed, meaning that wildcards can be part of patterns but cannot be part of processes. For example, we cannot write

    for( _ <- @Nil){ _!(Nil) }

due to the wildcard being in the body of the listen, even though it is correct to write

    for( x <- @Nil){ x!(Nil) }

It is, however, perfectly acceptable to write

    for( _ <- @Nil){ Nil }

That is the first distinction. The second distinction is that wildcards can bind to statements containing free variables, while variables cannot. For example, the pattern

    new x in { p }

will match to *strictly fewer* patterns than the pattern

    new x in { _ }

`new x in { p }` will only match to a process which does not ever use the new channel in the body, such as `new x in { Nil }`. Here, `p` binds to `Nil`. It cannot match to something like `new x in { x!(Nil) }`, because `p` would have to match with `x!(Nil)`, which by itself has the free variable `x`. On the other hand, `new x in { _ }` will match with `new x in { x!(Nil) }`, throwing away `x!(Nil)`. Since `_` doesn't actually bind to anything, we are allowed to give it things with free variables. We note here that the rules for patterns within patterns still apply, meaning that a wildcard cannot be used to match to a piece of a pattern, unless that pattern itself uses a wildcard in precisely the same way.
