/** org_edge_strg_symb( +Org, -Symb1, Symb2, W )

Organism abstraction interface to STRING database access.

==
?- org_edge_strg_symb( hs, 'LMTK3', Symb2, W ).
Symb2 = 'AATK',
W = 359 
...

?- org_edge_strg_symb( mouse, 'Ltmk3', 'Aatk', W ).
W = 308.
==

@author nicos angelopoulos
@version  0:1 2019/4/8

*/
org_edge_strg_symb( Org, Symb1, Symb2, W ) :-
    sort( [Symb1,Symb2], [SymbA,SymbB] ),
    org_edge_strg_symb_ord( Org, SymbA, SymbB, W ).

org_edge_strg_symb_ord( human, SymbA, SymbB, W ) :-
    org_edge_strg_symb_ord( hs, SymbA, SymbB, W ).
org_edge_strg_symb_ord( hs, SymbA, SymbB, W ) :-
	edge_strg_hs_symb(SymbA,SymbB,W).
org_edge_strg_symb_ord( mouse, SymbA, SymbB, W ) :-
	edge_strg_mouse_symb(SymbA,SymbB,W).
