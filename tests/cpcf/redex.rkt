#lang racket
(require tests/pcf/make-tests
         tests/cpcf/make-tests
         cpcf/redex)
(make-pcf-tests -->cv typable/contract?)
(make-cpcf-tests -->cv typable/contract?)
