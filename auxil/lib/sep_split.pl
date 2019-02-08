/**  sep_split( +Sep, +Full, -Parts ).

A rehash of atomic_list_concat/3 arguments suitable for meta-calling 
from csv_ids_map/6. If result is singleton, list is stripped.

Predicate fails for empty atom ('') Full.

==
?- sep_split( '|', 'NCRNA00181|A1BGAS|A1BG-AS', Parts ).
Parts = ['NCRNA00181', 'A1BGAS', 'A1BG-AS'].
?- sep_split( '|', 'CPAMD9', Parts ).
Parts = 'CPAMD9'.
==

@author nicos angelopoulos
@version  0.1 2019/2/8

*/
sep_split( _Sep, '', [] ) :- !.
sep_split( Sep, Full, Parts ) :-
    Full \== [],
    atomic_list_concat( PartsPrv, Sep, Full ),
    ( PartsPrv = [Parts] -> true; Parts = PartsPrv ).
