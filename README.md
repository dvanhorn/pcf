Symbolic PCF
============

This collection provides an abstract reduction semantics for PCF where
components may be abstracted to their type, written `(• T)`.  See
`pcf.rkt` for a Redex model of plain PCF; see `spcf.rkt` for the
extension to the symbolic semantics; see `cpcf.rkt` for the extension
to contracts.  These languages are also available as `#lang`
languages and include static type checking.

To install using `raco git` [1]:

   `% raco git --github dvanhorn pcf`

Enables:

```
#lang pcf <option>
#lang pcf/symbolic <option>
#lang pcf/contracts <option>
```

where

`option ::=
        | traces
        | stepper`

See `examples/` for examples.

[1] To install `raco git`:

```
% git clone http://github.com/samth/raco-git.git
% raco link raco-git
% raco setup raco-git
```