#lang racket
(require tests/pcf/make-tests cpcf/redex)
(make-pcf-tests -->cv typable/contract?)
