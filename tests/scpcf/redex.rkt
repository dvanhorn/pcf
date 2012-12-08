#lang racket
(require tests/pcf/make-tests
         tests/spcf/make-tests
         tests/cpcf/make-tests
         tests/scpcf/make-tests
         scpcf/redex)
(make-pcf-tests -->scv typable/contract/symbolic?)
(make-spcf-tests -->scv typable/contract/symbolic?)
(make-scpcf-tests -->scv typable/contract/symbolic?)
