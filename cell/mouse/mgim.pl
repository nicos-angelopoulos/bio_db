:- module( bio_db_mouse_mgim, [
                bio_db_mouse_mgim/0,
                %       + MGI database: fixme: url
                map_mgim_mouse_mgim_chrl/5,
                map_mgim_mouse_mgim_genb/2,
                map_mgim_mouse_mgim_symb/2,
                map_mgim_mouse_mgim_unip/2,
                map_mgim_mouse_symb_wdra/2,
                map_mgim_mouse_syno_mgim/2
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

% :- dynamic( map_mgim_mouse_mgim_symb/2 ).

/**  bio_db_mouse_mgim.

Documentation predicate for mouse data from MGI database.
DNA fragments are referred to as markers in the database, thus
mgim is the 4 letter code for the database and its unique ids.

==
?- lib( &bio_db(mouse(mgim)) ).
?- [pack('bio_db/cell/mouse/mgim')].
==

@author nicos angelopoulos
@version  0.1 2018/11/3

*/
bio_db_mouse_mgim.

/**  map_mgim_mouse_mgim_chrl( +Mgim, -Chr, -Start, -End, -Strand ).

MGI marker to chromosomal location.

== 
?- map_mgim_mouse_mgim_symb(Mgim,'Lmtk3'), 
   map_mgim_mouse_mgim_chrl( Mgim, Chr, Start, End, Strand ).
==

*/
map_mgim_mouse_mgim_chrl( Mgi, Chr, Sta, End, Sgn ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_chrl(Mgi,Chr,Sta,End,Sgn) ).

/**  map_mgim_mouse_mgim_genb( +UniP, -GenB ).

Map predicate from MGI marker to Gene Bank ids.

==
?-  map_mgim_mouse_mgim_sybm( Mgim, 'Lmtk3' ),
    map_mgim_mouse_mgim_genb( Mgim, GenB ).

==
*/
map_mgim_mouse_mgim_genb( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_genb(X,Y) ).

/**  map_mgim_mouse_mgim_symb( +Mgim, -Symb ).

Map predicate between MGI marker and (MGI) symbols.

==
?- map_mgim_mouse_mgim_sybm( Mgim, 'Lmtk3' ).
==
*/
map_mgim_mouse_mgim_symb( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_symb(X,Y) ).

/**  map_mgim_mouse_mgim_unip( +Mgim, -Unip ).

Map predicate from MGI markder ids to Uniprot ids.

==
?- map_mgim_mouse_mgim_sybm( Mgim, 'Lmtk3' ),
   map_mgim_mouse_mgim_unip( MgiM, Unip ).

==
*/
map_mgim_mouse_mgim_unip( Sprt, Seqn ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_unip(Sprt,Seqn) ).
    
/**  map_mgim_mouse_symb_wdra( +Symb, -Wdra ).

Map predicate from MGI symbols to withdrawn names.

==
?- map_mgim_mouse_symb_wdra( S, W ).
==
*/
map_mgim_mouse_symb_wdra( Symb, Wdra ) :-
    bio_db:bio_db_serve( map_mgim_mouse_symb_wdra(Symb,Wdra) ).

/*  map_mgim_mouse_syno_mgim( +Syno, -Mgim ).

Map predicate from Synonym to MGI marker ID.

==
?- map_mgim_mouse_syno_mgim( Syno, MgiM ).
*/
map_mgim_mouse_syno_mgim( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_syno_mgim(X,Y) ).
