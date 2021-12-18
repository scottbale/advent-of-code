USING: arrays assocs io.files io.encodings.utf8 kernel math prettyprint sequences vectors ;
IN: day10

! delimiter characters and their number values
! ( 40
! [ 91
! [ 123
! < 60
! ) 41
! ] 93
! } 125
! > 62

! hash tables of constant data
CONSTANT: scores H{ 
{ CHAR: ) 3 } 
{ CHAR: ] 57 } 
{ CHAR: } 1197 } 
{ CHAR: > 25137 } }

CONSTANT: closing-scores H{ 
{ CHAR: ) 1 } 
{ CHAR: ] 2 } 
{ CHAR: } 3 } 
{ CHAR: > 4 } }

CONSTANT: delims H{ 
{ CHAR: ( CHAR: ) } 
{ CHAR: [ CHAR: ] } 
{ CHAR: { CHAR: } } 
{ CHAR: < CHAR: > } }

: closing? ( delim -- delim ? )
dup scores ?at nip ;

: scored? ( score-and-delim-stack-pair delim -- score-and-delim-stack-pair delim ? )
over first 0 > ;

! Calculate the score for the delim, update the score in the first slot of the pair
: score-corrupted ( score-and-delim-stack-pair delim -- updated-score-and-delim-stack-pair delim )
2dup scores at swap set-first ;

: cons-delim ( score-and-delim-stack-pair delim -- updated-pair delim )
2dup swap second push ;

: delims-match? ( stack-pair a-delim -- stack-pair a-delim ? )
2dup swap second last delims at = ;

: reduce-line ( score-and-delim-stack-pair next-delim -- updated-pair )
! If line is already scored, then it's already been found to be corrupted. No further reduction
! necessary.
scored? not
! then quotation
[ 
    ! if delim is a closing delimiter
    closing?
    ! then quotation
    [ 
        ! if
        delims-match?
        ! then pop one from delim-queue
        [ over second pop drop ]
        ! else corrupt line detected - score next-delim
        [ score-corrupted ]
        if
    ]
    ! else append next-delim to the delim-stack
    [ cons-delim ]
    if
] 
when
drop ;

! Process a line of input, return a pair [score of zero or more, unclosed open-delimiter stack]
: score-line ( line -- line-score-pair )
0 V{ } 2array [ reduce-line ] reduce ;

! Sum the score of all corrupted input lines
: runner-pt1 ( input -- answer )
[ score-line first ] map-sum ;

! ------- Send input to runner -------------------------------------------

: run ( -- )
"day10.txt" utf8 file-lines runner-pt1 . ; ! 319233

CONSTANT: test-input { "[({(<(())[]>[[{[]{<()<>>"
                       "[(()[<>])]({[<{<<[]>>("
                       "{([(<{}[<>[]}>{[]{[(<()>"
                       "(((({<>}<{<{<>}{[]{[]{}"
                       "[[<[([]))<([[{}[[()]]]"
                       "[{[{({}]{}}([{[{{{}}([]"
                       "{<[[]]>}<{[{[{[]{()[[[]"
                       "[<(<(<(<{}))><([]([]()"
                       "<{([([[(<>()){}]>(<<{{"
                       "<{([{{}}[<[[[<>{}]]]>[]]" } 

: run-sample ( -- )
test-input runner-pt1 . ; ! 26397
