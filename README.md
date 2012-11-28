PCF, 'PCF, CPCF, 'CPCF
======================

This collection implements four core programming languages, useful for
studying PCF, contracts, and symbolic execution.

The four languages are the following:

* PCF - a core typed language (with errors and recursion)
* Symbolic PCF ('PCF) - an extension of PCF endowed with a notion
  of "symbolic values", written `(• T)`, which represents an
  abstraction of all values of type `T`.
* Contract PCF (CPCF) - an extension of PCF endowed with behavioral
  software contracts.  Contracts include arbitrary predicates written
  in PCF and higher-order contracts, written `(C ... -> C)`.  The
  monitor of a contract against a computation is written `(C ⚖ M)`.
  When a contract fails, `blame` is signalled (although this
  simplified model does not indicate _who_ is to blame).
* Symbolic CPCF ('CPCF) - an extension of Contract PCF endowed with
  symbolic values written `(• T C ...)`, which represents an
  abstraction of all values of type `T` satisfying contracts `C ...`.

See `pcf.rkt` for a Redex model of plain PCF; see `spcf.rkt` for the
extension to the symbolic semantics; see `cpcf.rkt` for the extension
to contracts; see `scpcf.rkt` for 'CPCF.

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
