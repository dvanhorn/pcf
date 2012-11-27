Symbolic PCF
============

This collection provides an abstract reduction semantics for PCF where
components may be abstracted to their type, written `(• T)`.  See
`pcf.rkt` for a Redex model of plain PCF; see `spcf.rkt` for the
extension to the symbolic semantics; see `cpcf.rkt` for the extension
to contracts.  These languages are also available as `#lang`
languages and include static type checking.

To install:

   `% git clone http://github.com/dvanhorn/pcf`
   `% raco link pcf`
   `% raco setup pcf`

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
