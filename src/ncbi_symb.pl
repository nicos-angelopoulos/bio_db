/** ncbi_symb( +Org, ?Ncbi, ?Symb ).

Organism level interface for mapping between gene Entrez ids and Symbols.

==
?- ncbi_symb( hs, Ncbi, 'LMTK3' ).
Ncbi = 114783.

?- ncbi_symb( mouse, Ncbi, 'Lmtk3' ).
Ncbi = 381983.
==

@author nicos angelopoulos
@version  0:1 2019/4/7
@version  0:2 2022/12/29 entz -> ncbi

*/
ncbi_symb( human, Ncbi, Symb ) :-
    ncbi_symb( hs, Ncbi, Symb ).

ncbi_symb( hs, Ncbi, Symb ) :-
    hgnc_homs_ncbi_symb( Ncbi, Symb ).
ncbi_symb( mouse, Ncbi, Symb ) :-
    ncbi_symb_mouse( Ncbi, Symb ).
ncbi_symb( chicken, Ncbi, Symb ) :-
    cgnc_galg_cgnc_ncbi( Cgnc, Ncbi ),
    cgnc_galg_cgnc_symb( Cgnc, Symb ).

ncbi_symb_mouse( Ncbi, Symb ) :-
    ( ground(Ncbi) ; ground(Symb) ),
    map_mgim_mouse_mgim_ncbi( Mgim, Ncbi ),
    map_mgim_mouse_mgim_symb( Mgim, Symb ),
    !.
ncbi_symb_mouse( Ncbi, Symb ) :-
    map_mgim_mouse_mgim_ncbi( Mgim, Ncbi ),
    map_mgim_mouse_mgim_symb( Mgim, Symb ).
