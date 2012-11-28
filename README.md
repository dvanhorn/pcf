PCF with Contracts and Symbolic Values
======================================

This collection implements four core programming languages, useful for
studying PCF, contracts, and symbolic execution.

The four languages are the following:

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

See `pcf.rkt`, `spcf.rkt`, `cpcf.rkt`, and `scpcf.rkt` for the Redex
models of PCF, 'PCF, CPCF, and 'CPCF, respectively.

These languages are also available as `#lang` languages and include
static type checking.

To install:

```
% git clone http://github.com/dvanhorn/pcf
% raco link pcf
% raco setup pcf
```

Enables:

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

See `examples/` for examples.
