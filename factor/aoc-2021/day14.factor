USING: arrays assocs io.files io.encodings.utf8 kernel math prettyprint sequences splitting vectors ;
IN: day14

! : ->map ( 

: runner-pt1 ( input -- answer )
dup drop ;

! ------- Send input to runner -------------------------------------------

: run ( -- )
"day14.txt" utf8 file-lines runner-pt1 . ;

CONSTANT: test-input { "NNCB"
                       ""
                       "CH -> B"
                       "HH -> N"
                       "CB -> H"
                       "NH -> C"
                       "HB -> C"
                       "HC -> B"
                       "HN -> C"
                       "NN -> C"
                       "BH -> H"
                       "NC -> B"
                       "NB -> B"
                       "BN -> B"
                       "BB -> N"
                       "BC -> B"
                       "CC -> N"
                       "CN -> C" }

: run-sample ( -- )
test-input runner-pt1 . ;


! H{ { "CH" { "CB" "BH" } }
!    { "HH" { "HN" "NH" } } }
