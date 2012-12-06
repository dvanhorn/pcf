1135
((3) 0 () 4 ((q lib "pcf/redex.rkt") (q lib "cpcf/redex.rkt") (q lib "scpcf/redex.rkt") (q lib "spcf/redex.rkt")) () (h ! (equal) ((c form c (c (? . 0) q δ)) q (80 . 2)) ((c def c (c (? . 0) q v)) q (13 . 2)) ((c form c (c (? . 2) q typeof/contract/symbolic)) q (628 . 2)) ((c form c (c (? . 0) q PCF)) q (0 . 2)) ((c form c (c (? . 1) q CPCF-source)) q (188 . 2)) ((c form c (c (? . 1) q CPCF)) q (209 . 2)) ((c form c (c (? . 2) q SCPCF)) q (613 . 2)) ((c def c (c (? . 1) q typable/contract?)) q (322 . 3)) ((c def c (c (? . 3) q -->sv)) q (449 . 2)) ((c form c (c (? . 3) q SPCF)) q (402 . 2)) ((c form c (c (? . 3) q typeof/symbolic)) q (503 . 2)) ((c def c (c (? . 1) q cv)) q (223 . 2)) ((c form c (c (? . 0) q typeof)) q (97 . 2)) ((c def c (c (? . 0) q typable?)) q (118 . 3)) ((c def c (c (? . 0) q -->v)) q (45 . 2)) ((c def c (c (? . 3) q sv)) q (416 . 2)) ((c def c (c (? . 3) q typable/symbolic?)) q (533 . 3)) ((c form c (c (? . 1) q typeof/contract)) q (292 . 2)) ((c def c (c (? . 2) q typable/contract/symbolic?)) q (667 . 3)) ((c def c (c (? . 1) q -->cv)) q (256 . 2)) ((c form c (c (? . 3) q δ^)) q (485 . 2))))
language
PCF
value
v : reduction-relation?
value
-->v : reduction-relation?
judgment-form
δ
judgment form
typeof
procedure
(typable? m) -> boolean?
  m : (redex-match PCF M)
language
CPCF-source
language
CPCF
value
cv : reduction-relation?
value
-->cv : reduction-relation?
judgment form
typeof/contract
procedure
(typable/contract? m) -> boolean?
  m : (redex-match CPCF M)
language
SPCF
value
sv : reduction-relation?
value
-->sv : reduction-relation?
judgment-form
δ^
judgment form
typeof/symbolic
procedure
(typable/symbolic? m) -> boolean?
  m : (redex-match SPCF M)
language
SCPCF
judgment form
typeof/contract/symbolic
procedure
(typable/contract/symbolic? m) -> boolean?
  m : (redex-match SCPCF M)
