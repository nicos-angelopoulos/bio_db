:- module( bio_db_homs_strg, [
                bio_db_homs_strg/0,
                %       + String
                strg_homs_edge_ensp/3,
                strg_homs_edge_symb/3
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_strg.

Documentation predicate for Homo sapiens data from String database.

==
?- lib( &bio_db(homs(strg)) ).
?- [ pack('bio_db/cell/homs/strg') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/25

*/
bio_db_homs_strg.

/** strg_homs_edge_ensp( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
Symb = 'LMTK3'.
?- hgnc_homs_symb_ncbi('LMTK3',Ncbi),ncbi_homs_ncbi_ensp(Ncbi,EnsP),strg_homs_edge_ensg( EnsP, Inter, W ).
Ncbi = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000075503',
W = 186 ;
Ncbi = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000162044',
W = 165 ;
Ncbi = 114783,
EnsP = 'ENSP00000270238',
Inter = 'ENSP00000178640',
W = 389
...
==
*/
strg_homs_edge_ensp( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_homs_edge_ensp(X,Y,Z) ).

/** strg_homs_edge_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between HGNC symbol ids.
W is an integger in 0 < W < 1000. Note that Symb1 @< Symb2,
so you need to query with Symb2 (or Symb1) in both positions if Symb1 (or Symb2)
are unbound.

==
?- strg_homs_edge_symb( 'LMTK3', Inter, W ).
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

?- strg_homs_edge_symb( Inter, 'LMTK3', W ).
Inter = 'AATK',
W = 170 ;
Inter = 'ACTRT1',
W = 150 ;

==
*/
strg_homs_edge_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_homs_edge_symb(X,Y,Z) ).
