:- module( bio_db_suss_strg, [
                bio_db_suss_strg/0,
                %       + String
                strg_suss_edge_ensp/3,
                strg_suss_edge_symb/3,
                strg_suss_ensp_symb/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_suss_strg.

Documentation predicate for pig (sus scrofa) data from String database.

==
?- lib( &bio_db(suss(strg)) ).
?- [ pack('bio_db/cell/suss/strg') ].
==

@author nicos angelopoulos
@version  0.1 2023/6/2

*/
bio_db_suss_strg.

/**  strg_suss_edge_ensp( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.

W is an integer in 0 < W < 1000.
==
?- strg_suss_edge_ensp(Ensp1,Ensp2, W).
==

@author nicos angelopoulos
@version  0.1 2023/6/2
*/
strg_suss_edge_ensp( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_suss_edge_ensp(X,Y,Z) ).

/**  strg_suss_edge_symb(?Symb1, ?Symb2, ?W).

Weighted graph edges predicate from String database between symbols.

==
?- strg_suss_edge_symb(A,B,C).
A = 'A1BG',
B = 'A2M',
C = 317 ;
A = 'A1BG',
B = 'A4GNT',
C = 198 ...
==

@author nicos angelopoulos
@version  0.1 2023/6/2
*/
strg_suss_edge_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_suss_edge_symb(X,Y,Z) ).

/**  strg_suss_ensp_symb(?Ensp, ?Symb).

Ensembl protein to Symbol- acccording to String database.

==
?- strg_suss_ensp_symb(Ensp, Symb).
==

@author nicos angelopoulos
@version  0.1 2023/6/2

*/
strg_suss_ensp_symb( X, Y ) :-
    bio_db:bio_db_serve( strg_suss_ensp_symb(X,Y) ).
