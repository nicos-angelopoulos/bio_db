/** is_symbol( +Org, ?Symbol ).

True iff Symbol is a gene symbol of Organism.

==
?- is_symbol( hs, 'LMTK3' ).
true.

?- is_symbol( mouse, 'Lmtk3' ).
true.
==

@author nicos angelopoulos
@version  0:1 2019/4/7

*/
is_symbol( human, Symb ) :-
    is_symbol( hs, Symb ).
is_symbol( hs, Symb ) :-
    map_hgnc_hgnc_symb( _, Symb ).
is_symbol( mouse, Symb ) :-
    map_mgim_mouse_mgim_symb( _, Symb ).
