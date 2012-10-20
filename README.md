Symbolic PCF
============

This collection provides an abstract reduction semantics for PCF where
components may be abstracted to their type, written (• T).  See
`pcf.rkt` for a Redex model of plain PCF; see `spcf.rkt` for the
extension to symbolic semantics.  Both languages are also available as
`#lang` languages.

To install:

   `% raco git --github dvanhorn pcf`

Enables:

`#lang pcf <option>`
`#lang pcf/symbolic <option>`

where

`option ::=
        | traces
        | stepper`

See `examples/` for examples.