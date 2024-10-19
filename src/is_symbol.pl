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
@tbd this needs to be more robust... needs also to be updated when new organisms are updated

*/
is_symbol( chicken, Symb ) :-
     cgnc_galg_cgnc_symb( _, Symb ).
is_symbol( human, Symb ) :-
    is_symbol( hs, Symb ).
is_symbol( hs, Symb ) :-
    map_hgnc_hgnc_symb( _, Symb ).
is_symbol( mouse, Symb ) :-
    map_mgim_mouse_mgim_symb( _, Symb ).
is_symbol( pig, Symb ) :-
     ncbi_suss_ncbi_symb( _, Symb ).
