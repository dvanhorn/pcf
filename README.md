PCF with Contracts and Symbolic Values
======================================

This collection implements four core programming languages, useful for
studying PCF, contracts, and symbolic execution:

* __PCF__: a core typed language (with natural numbers, errors and
  recursion).

* __Symbolic PCF ('PCF)__: an extension of PCF endowed with a notion
  of "symbolic values", written `(• T)`, which represents an
  abstraction of all values of type `T`.

* __Contract PCF (CPCF)__: an extension of PCF endowed with
  behavioral software contracts.  Contracts include arbitrary
  predicates written in PCF and higher-order contracts, written `(C
  ... -> C)`.  The monitor of a contract against a computation is
  written `(C ⚖ M)`.  When a contract fails, `blame` is signalled
  (although this simplified model does not indicate _who_ is to
  blame).

* __Symbolic CPCF ('CPCF)__: an extension of Contract PCF endowed with
  symbolic values written `(• T C ...)`, which represents an
  abstraction of all values of type `T` satisfying contracts `C ...`.

These languages are available as `#lang` languages that include static
type checking:

```
#lang pcf <option>
#lang pcf/symbolic <option>
#lang pcf/contracts <option>
#lang pcf/contracts/symbolic <option>
```

where

`option ::=
        | traces
        | stepper`

These languages are also available as Redex models.

## Organization

* `examples/`: examples written in each language.
* `redex/`: Redex models of each language.
* `lang/`: implements `#lang pcf`.
* `symbolic/lang`: implements `#lang pcf/symbolic`.
* `contracts/lang`: implements `#lang pcf/contracts`.
* `contracts/symbolic/lang`: implements `#lang pcf/contracts/symbolic`.
* `langs/`: common code for creating languages.

## Installation and testing

You will need [Racket](http://racket-lang.org/) 5.3.1 or later.

```
% git clone http://github.com/dvanhorn/pcf
% raco link pcf
% raco setup pcf
% raco test pcf
```

The last command tests every file in collection.  It will likey create
a bunch of windows and print results of examples, but so long as it
does not signal an error, the tests have passed.
