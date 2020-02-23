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

Currently only untyped lambda calculus is supported. The syntax used in _adiram_ replaces the lambda sign by `\` and does not allow currying. Thus `\xy.x` is not valid and has to be written like `\x.\y.x`.

Variable names may consist of more than one ASCII letter and digit but have to start with a letter.

After starting up _adiram,_ you can enter expressions and they will be evaluated both by call-by-value and normal order strategies.

A session evaluating the Church numeral for 1 could like this:

```console
λ \s.\z.s z
\s.\z.s z
Call-by-value yields: λs.λz.s z
Normal order yields:  λs.λz.s z
```

There is syntactic sugar for numerals, you can just type the natural number in decimal digits and it will be resolved to its Church encoding. The following expression is equivalent to the previous run:

```console
λ 1
\s.\z.s z
Call-by-value yields: λs.λz.s z
Normal order yields:  λs.λz.s z
```

## Defining and importing expressions

You can define your own expressions and use them afterwards. The syntax is `:def 〈name〉 〈expression〉`. The name has to follow the rules for variable names.

The next example shows how to define booleans and a zero-check for Church numerals.

```console
λ :def tru \t.\f.t
λ :def fls \t.\f.f
λ :def is0 \n.n (\x.fls) tru
λ is0 1
is0 λs.λz.s z
Call-by-value yields: λt.λf.(λt'.λf'.t') f t
Normal order yields:  λt.λf.f
```

To facilitate definitions we allow to load files. The syntax is `:load 〈path〉`. The lines in the file will be run just like in the REPL.

With the supplied `std.txt` file the previous example could have been run like this:

```console
λ :load std.txt
λ is0 1
is0 λs.λz.s z
Call-by-value yields: λt.λf.(λt'.λf'.t') f t
Normal order yields:  λt.λf.f
```
