/** entz_symb( +Org, ?Entz, ?Symb ).

Organism level interface for mapping between gene Entrez ids and Symbols.

==
?- entz_symb( hs, Entz, 'LMTK3' ).
Entz = 114783.

?- entz_symb( mouse, Entz, 'Lmtk3' ).
Entz = 381983.
==

@author nicos angelopoulos
@version  0:1 2019/4/7

*/
entz_symb( human, Entz, Symb ) :-
    entz_symb( hs, Entz, Symb ).

entz_symb( hs, Entz, Symb ) :-
    map_hgnc_entz_symb( Entz, Symb ).

entz_symb( mouse, Entz, Symb ) :-
    entz_symb_mouse( Entz, Symb ).

entz_symb_mouse( Entz, Symb ) :-
    ( ground(Entz) ; ground(Symb) ),
    map_mgim_mouse_mgim_entz( Mgim, Entz ),
    map_mgim_mouse_mgim_symb( Mgim, Symb ),
    !.
entz_symb_mouse( Entz, Symb ) :-
    map_mgim_mouse_mgim_entz( Mgim, Entz ),
    map_mgim_mouse_mgim_symb( Mgim, Symb ).
