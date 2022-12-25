:- module( bio_db_hs_strg, [
                bio_db_hs_strg/0,
                %       + String
                edge_strg_hs/3,
                edge_strg_hs_symb/3
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_hs_strg.

Documentation predicate for Homo sapiens data from String database.

==
?- lib( &bio_db(hs(strg)) ).
?- [ pack('bio_db/cell/hs/strg') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_strg.

/**  edge_strg_hs( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
Symb = 'LMTK3'.
 map_hgnc_symb_entz( 'LMTK3', Entz ), map_ncbi_entz_ensp( Entz, EnsP ), edge_strg_hs( EnsP, Inter, W ).
Entz = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000075503',
W = 186 ;
Entz = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000162044',
W = 165 ;
Entz = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000178640',
W = 389 ...
==
*/
edge_strg_hs( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_hs(X,Y,Z) ).

/**  edge_strg_hs_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between HGNC symbol ids.
W is an integger in 0 < W < 1000. Note that Symb1 @< Symb2,
so you need to query with Symb2 (or Symb1) in both positions if Symb1 (or Symb2)
are unbound.

==
?- edge_strg_hs_symb( 'LMTK3', Inter, W ).
Inter = 'MAP2K5',
W = 389 ;
Inter = 'MAPK3',
W = 157 ;
Inter = 'MASTL',
W = 211 ;
Inter = 'MDC1',
W = 198 ;
Inter = 'MFSD2A',
W = 165 ;
Inter = 'MRPS30',
W = 179 ....

?- edge_strg_hs_symb( Inter, 'LMTK3', W ).
Inter = 'AATK',
W = 170 ;
Inter = 'ACTRT1',
W = 150 ;

==
*/
edge_strg_hs_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_hs_symb(X,Y,Z) ).
