/**  pfx_by( +Rmv, +Pfx, +Full, -Rem ).

Trivial pred for ensuring or removing DB prefixes from identifiers.

==
?- pfx_by( true, 'MGI:', 'MGI:ABC', 'ABC' ).
?- pfx_by( false, 'MGI:', 'MGI:ABC', 'MGI:ABC' ).
==

@author nicos angelopoulos
@version  0.1 2018/11/2

*/
pfx_by( true, Pfx, Full, Rem ) :-
    atom_concat( Pfx, Rem, Full ).
pfx_by( false, Pfx, Full, Full ) :-
    atom_concat( Pfx, _, Full ).
