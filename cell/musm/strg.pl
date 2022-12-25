:- module( bio_db_musm_strg, [
                bio_db_musm_strg/0,
                %       + String
                strg_musm_edge_ensp/3,
                strg_musm_edge_symb/3
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_musm_strg.

Documentation predicate for mouse data from String database.

==
?- lib( &bio_db(musm(strg)) ).
?- [ pack('bio_db/cell/musm/strg') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/25

*/
bio_db_musm_strg.

/**  strg_musm_edge_ensp( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
==

*/
strg_musm_edge_ensp( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_musm_edge_ensp(X,Y,Z) ).

/**  strg_musm_edge_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between Mgi marker ids.
W is an integger in 0 < W < 1000. Note that Symb1 @< Symb2,
so you need to query with Symb2 (or Symb1) in both positions if Symb1 (or Symb2)
are unbound.

==
?- strg_musm_edge_symb( 'Lmtk3', Inter, W ).
...
==
*/
strg_musm_edge_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_musm_edge_symb(X,Y,Z) ).
