:- module( bio_db_mouse_strg, [
                bio_db_mouse_strg/0,
                %       + String
                edge_strg_mouse/3,
                edge_strg_mouse_symb/3
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_mouse_strg.

Documentation predicate for Homo sapiens data from String database.

==
?- lib( &bio_db(mouse(strg)) ).
?- [ pack('bio_db/cell/mouse/strg') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_mouse_strg.

/**  edge_strg_mouse( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
Symb = '...'.
 map_hgnc_symb_entz( 'LMTK3', Entz ), map_ncbi_entz_ensp( Entz, EnsP ), edge_strg_mouse( EnsP, Inter, W ).
...
==
*/
edge_strg_mouse( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_mouse(X,Y,Z) ).

/**  edge_strg_mouse_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between HGNC symbol ids.
W is an integger in 0 < W < 1000. Note that Symb1 @< Symb2,
so you need to query with Symb2 (or Symb1) in both positions if Symb1 (or Symb2)
are unbound.

==
?- edge_strg_mouse_symb( 'Lmtk3', Inter, W ).
...
==
*/
edge_strg_mouse_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_mouse_symb(X,Y,Z) ).
