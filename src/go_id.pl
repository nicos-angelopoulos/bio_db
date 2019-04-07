/** go_id( +GoAtom, -GoInt ).
    go_id( -GoAtom, +GoInt ).

Convert between full and integer representation of gene ontology ids.

bio_db changed its storing of go ids to integer as of 
bio_db_repo_version(19:04:07).

==
?- go_id( 'GO:0000002', Int ).
Int = 2.

?- go_id( Go, 2 ).
Go = 'GO:0000002'.
==

@author nicos angelopoulos
@version 2019/4/7

*/
go_id( Atm, Int ) :-
    ground( Atm ),
    !,
    atom_concat( 'GO:', IntAtm, Atm ),
    atom_number( IntAtm, Int ).
go_id( Atm, Int ) :-
    % shall we check Int ?
    number_codes( Int, Codes ),
    length( Codes, CodesLen ),
    PadLen is 7 - CodesLen,
    findall( 0'0, between(1,PadLen,_), ZeroCodes ),
    flatten( [0'G,0'O,0':,ZeroCodes,Codes], AtmCodes ),
    atom_codes( Atm, AtmCodes ).

/** go_id( +GoOrInt, -Go, -Int ).

Map a GO atom (eg GO:0000002 or Go integer (2) representation of a GO 
id to the atomic representation (Go) and integer one (Int).

==
?- go_id( 'GO:0000002', Go, Int ).
Go = 'GO:0000002',
Int = 2.

?- go_id( 2, Go, Int ).
Go = 'GO:0000002',
Int = 2.
==

@author nicos angelopoulos
@version 2019/4/7

*/
go_id( Goi, Go, Int ) :-
    integer( Goi ), 
    !,
    go_id( Go, Goi ),
    Goi = Int.
go_id( Goi, Go, Int ) :-
    atom_concat( 'GO:', IntAtm, Goi ),
    !,
    atom_number( IntAtm, Int ),
    Goi = Go.
go_id( Goi, _Go, _Int ) :-
    throw( not_a_go_id(Goi) ).
