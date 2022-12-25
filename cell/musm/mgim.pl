:- module( bio_db_mouse_mgim, [
                bio_db_mouse_mgim/0,
                %       + MGI database: fixme: url
                map_mgim_mouse_mgim_chrl/5,
                map_mgim_mouse_mgim_entz/2,
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

Mgim = 3039582,
Chr = 7,
Start = 45783738,
End = 45804144,
Strand =  (+).

==

*/
map_mgim_mouse_mgim_chrl( Mgi, Chr, Sta, End, Sgn ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_chrl(Mgi,Chr,Sta,End,Sgn) ).

/**  map_mgim_mouse_mgim_genb( +Mgim, -GenB ).

Map predicate from MGI marker to Gene Bank ids.

==
?- map_mgim_mouse_mgim_symb( Mgim, 'Lmtk3' ), 
   map_mgim_mouse_mgim_genb( Mgim, GenB ), write( GenB ), nl, fail.

AB288873
AI413247
AK029204
AW046037
BC057032
BC059845
BC083185
BC094377
BE981921
false.
?-  
==
*/
map_mgim_mouse_mgim_genb( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_genb(X,Y) ).

/**  map_mgim_mouse_mgim_entz( +Mgim, -Entz ).

Map predicate from MGI marker to NCBI, Entrez ids.

==
?-  map_mgim_mouse_mgim_symb( Mgim, 'Lmtk3' ),
    map_mgim_mouse_mgim_entz( Mgim, Entz ).

Entz = 381983,
Mgim = 3039582.

==
*/
map_mgim_mouse_mgim_entz( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_entz(X,Y) ).

/**  map_mgim_mouse_mgim_symb( +Mgim, -Symb ).

Map predicate between MGI marker and (MGI) symbols.

==
?- map_mgim_mouse_mgim_symb( Mgim, 'Lmtk3' ).
Mgim = 3039582.
==
*/
map_mgim_mouse_mgim_symb( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_symb(X,Y) ).

/**  map_mgim_mouse_mgim_unip( +Mgim, -Unip ).

Map predicate from MGI markder ids to Uniprot ids.

==
?- map_mgim_mouse_mgim_symb( Mgim, 'Lmtk3' ),
   map_mgim_mouse_mgim_unip( Mgim, Unip ).

Mgim = 3039582,
Unip = 'Q5XJV6'.
==
*/
map_mgim_mouse_mgim_unip( Sprt, Seqn ) :-
    bio_db:bio_db_serve( map_mgim_mouse_mgim_unip(Sprt,Seqn) ).
    
/**  map_mgim_mouse_symb_wdra( +Symb, -Wdra ).

Map predicate from MGI symbols to withdrawn names.

==
?- map_mgim_mouse_symb_wdra( S, W ).
S = 15006,
W = 'H2-Q1' ;
...
==
*/
map_mgim_mouse_symb_wdra( Symb, Wdra ) :-
    bio_db:bio_db_serve( map_mgim_mouse_symb_wdra(Symb,Wdra) ).

/*  map_mgim_mouse_syno_mgim( +Syno, -Mgim ).

Map predicate from Synonym to MGI marker ID.

==
?- map_mgim_mouse_mgim_symb( Mgim, 'Lmtk3' ),
|    map_mgim_mouse_syno_mgim( Syno, Mgim ).

Mgim = 3039582,
Syno = 'AATYK3' ;
Mgim = 3039582,
Syno = 'Aatyk3'.
==

*/
map_mgim_mouse_syno_mgim( X, Y ) :-
    bio_db:bio_db_serve( map_mgim_mouse_syno_mgim(X,Y) ).
