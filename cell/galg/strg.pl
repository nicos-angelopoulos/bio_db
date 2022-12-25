:- module( bio_db_galg_strg, [
                bio_db_galg_strg/0,
                %       + String
                strg_galg_edge_ensp/3,
                strg_galg_edge_symb/3,
                strg_galg_ensp_symb/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_galg_strg.

Documentation predicate for chicken (gallus gallus) data from String database.

Note that Sting ensembl protein ids do not seem to match those of cgnc so
you should only use maps_strg_galg_ensp_symb/2 in this context.

==
?- lib( &bio_db(galg(strg)) ).
?- [ pack('bio_db/cell/galg/strg') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_galg_strg.

/**  strg_galg_edge_ensp( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
?- 
     strg_galg_edge_ensp( Ensp1, Ensp2, 577 ).
Ensp1 = 'ENSGALP00000000003',
Ensp2 = 'ENSGALP00000016521' ;
...
==

@author nicos angelopoulos
@version  0.1 2022/12/19
*/
strg_galg_edge_ensp( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_galg_edge_ensp(X,Y,Z) ).

/**  strg_galg_edge_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between symbols.

Symbols are derived from ensembl proteins (which doesn't seem to be CGNC compatible).
Thus, we are using map_strg_galg_ensp_symb/2 for the conversion.

==
?- strg_galg_edge_symb( 'Lmtk3', Inter, W ).

==

@author nicos angelopoulos
@version  0.1 2022/12/19
*/
strg_galg_edge_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( strg_galg_edge_symb(X,Y,Z) ).

/**  strg_galg_ensp_symb( Ensp, Symb ).

==
?- strg_galg_ensp_symb( 'ENSGALP00000000003', Symb ).
Symb = 'RFK'.

==

Compare with: 
==
?- cgnc_galg_cgnc_symb(Cgnc, 'RFK' ).
false.
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
strg_galg_ensp_symb( X, Y ) :-
    bio_db:bio_db_serve( strg_galg_ensp_symb(X,Y) ).
