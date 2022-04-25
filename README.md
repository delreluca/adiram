# adiram

[![Build Status](https://travis-ci.org/delreluca/adiram.svg?branch=master)](https://travis-ci.org/delreluca/adiram)

A small lambda calculus interpreter in Haskell.

It is a personal learning project to explore both lambda calculus and Haskell.

## Installation and starting

It is recommended to use [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) to build _adiram._ It will sandbox GHC and the necessary packages for you.
The following commands will build and run tests and start up the _adiram_ REPL. You can type `:q` to quit.

```console
$ stack test
…
All 1366 tests passed (0.14s)

adiram> Test suite adiram-test passed
$ stack exec adiram-exe
Welcome to adiram
λ
```

## Syntax and evaluation

Currently only untyped lambda calculus is supported. The syntax used in _adiram_ replaces the lambda sign by `\` and does not allow multiple bindings instead of currying. Thus `\x y.x` is not valid and has to be written like `\x.\y.x`.

Variable names may consist of more than one ASCII letter and digit but have to start with a letter.

After starting up _adiram,_ you can enter expressions and they will be evaluated both by call-by-value and normal order strategies.

A session evaluating the Church numeral for 1 could like this:

```console
λ \s.\z.s z
\s.\z.s z
Normal order yields:  λs.λz.s z
```

### Evaluation strategies

As you can see, normal order is the default evaluation strategy.

You can prefix the expression by `!`, followed by flags, to control the evaluation. If you want to use call-by-value evaluation you can supply the flag `v`. If you want both normal order and call-by-value you can use `!nv`. If you only want the expression pretty-printed back use `!p` to disable evaluation.

### Iteration limits

With references to free variables (see below) it is possible to generate infinite regressions. To break them a default iteration limit of 10000 is in place. To change the limit use the command `:set maxiter 〈new-limit〉`.

### Church encoding: Syntactic sugar and helpers

There is syntactic sugar for numerals and lists, you can just type the natural number in decimal digits and it will be resolved to its Church encoding, using `s` for _succ_ and `z` for 0. The following expression is equivalent to the previous run:

```console
λ !nv 1
\s.\z.s z
Call-by-value yields: λs.λz.s z
Normal order yields:  λs.λz.s z
```

You can also print the numeric value of a Church encoded result. To activate this feature use the flag `z`, for example

```console
λ !zn \s.\z.s (s z)
λs.λz.s (s z)
Normal order yields:  λs.λz.s (s z)
 Church numeral: 2
```

For lists embrace the list in square brackets and separated its members by commas. The list will be desugared into its Church encoding using `c` as the _cons_ binder and `n` for _nil._ Should names clash they will be suffixed by apostrophes. For example:

```console
λ !nv [A,B,C]
λc.λn.c A (c B (c C n))
Call-by-value yields: λc.λn.c A (c B (c C n))
Normal order yields:  λc.λn.c A (c B (c C n))
```

## Defining and importing expressions

You can define your own expressions and use them afterwards. The syntax is `:def 〈name〉 〈expression〉`. The name has to follow the rules for variable names.

It is possible to compare the result against known variables by supplying the lookup flag `l`, e.g. `!nl` for normal order evaluation.

The next example shows how to define booleans and a zero-check for Church numerals.

```console
λ :def tru \t.\f.t
λ :def fls \t.\f.f
λ :def is0 \n.n (\x.fls) tru
λ !nv is0 1
is0 λs.λz.s z
Call-by-value yields: λt.λf.f
Normal order yields:  λt.λf.f
```

To facilitate definitions we allow to load files. The syntax is `:load 〈path〉`. The lines in the file will be run just like in the REPL.

With the supplied `std.txt` file the previous example could have been run like this, also showing the lookup feature:

```console
λ !nvl is0 1
is0 λs.λz.s z
Call-by-value yields: λt.λf.tru f t
Normal order yields:  λt.λf.f
This is equivalent to fls
```

The difference in call-by-value comes from the definition of `fls` as `not tru` in the supplied file.

## Naming of free variables

**NOTE:** The supplied functions will make use of abstractions that bind lowercase characters `t`, `f`, `p`, `m`, `n`, `s`, `z`, `c`, `n`, `x`, `y` for creating new booleans, numbers or lists. This can have unintended effects when you use lowercase characters for free variables. You can use uppercase characters or longer variable names instead.
