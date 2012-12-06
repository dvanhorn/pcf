1272
((3) 0 () 4 ((q lib "spcf/redex.rkt") (q lib "pcf/redex.rkt") (q lib "cpcf/redex.rkt") (q lib "scpcf/redex.rkt")) () (h ! (equal) ((c form c (c (? . 0) q typeof/symbolic)) q (562 . 2)) ((c form c (c (? . 1) q δ)) q (80 . 2)) ((c def c (c (? . 0) q -->sv)) q (488 . 2)) ((c form c (c (? . 0) q inj-sv)) q (542 . 2)) ((c form c (c (? . 1) q typeof)) q (116 . 2)) ((c form c (c (? . 2) q typeof/contract)) q (331 . 2)) ((c def c (c (? . 1) q -->v)) q (45 . 2)) ((c def c (c (? . 2) q typable/contract?)) q (361 . 3)) ((c def c (c (? . 0) q sv)) q (455 . 2)) ((c form c (c (? . 2) q CPCF-source)) q (207 . 2)) ((c form c (c (? . 0) q SPCF)) q (441 . 2)) ((c def c (c (? . 0) q typable/symbolic?)) q (592 . 3)) ((c def c (c (? . 2) q cv)) q (242 . 2)) ((c def c (c (? . 3) q typable/contract/symbolic?)) q (726 . 3)) ((c form c (c (? . 3) q typeof/contract/symbolic)) q (687 . 2)) ((c def c (c (? . 1) q typable?)) q (137 . 3)) ((c form c (c (? . 0) q δ^)) q (524 . 2)) ((c form c (c (? . 1) q PCF)) q (0 . 2)) ((c def c (c (? . 2) q -->cv)) q (275 . 2)) ((c form c (c (? . 2) q inj-cv)) q (311 . 2)) ((c form c (c (? . 1) q inj-v)) q (97 . 2)) ((c def c (c (? . 1) q v)) q (13 . 2)) ((c form c (c (? . 2) q CPCF)) q (228 . 2)) ((c form c (c (? . 3) q SCPCF)) q (672 . 2))))
language
PCF
value
v : reduction-relation?
value
-->v : reduction-relation?
judgment-form
δ
metafunction
inj-v
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
metafunction
inj-cv
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
metafunction
inj-sv
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
