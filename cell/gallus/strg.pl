:- module( bio_db_gallus_strg, [
                bio_db_gallus_strg/0,
                %       + String
                edge_strg_gallus/3,
                edge_strg_gallus_symb/3,
                map_strg_gallus_ensp_symb/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_gallus_strg.

Documentation predicate for chicken (gallus gallus) data from String database.

Note that Sting ensembl protein ids do not seem to match those of cgnc so
you should only use maps_strg_gallus_ensp_symb/2 in this context.

==
?- lib( &bio_db(gallus(strg)) ).
?- [ pack('bio_db/cell/gallus/strg') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_gallus_strg.

/**  edge_strg_gallus( ?EnsP1, ?EnsP2, ?W ).

Weighted graph edges predicate from String database between Ensembl protein ids.
W is an integer in 0 < W < 1000.

==
?-
     edge_strg_gallus( Ensp1, Ensp2, 577 ).

?- edge_strg_gallus( Ensp1, Ensp2, 577 ).
Ensp1 = 'ENSGALP00000000003',
Ensp2 = 'ENSGALP00000016521' ;
...
==

@author nicos angelopoulos
@version  0.1 2022/12/19
*/
edge_strg_gallus( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_gallus(X,Y,Z) ).

/**  edge_strg_gallus_symb( ?Symb1, ?Symb2, ?W ).

Weighted graph edges predicate from String database between symbols.

Symbols are derived from ensembl proteins (which doesn't seem to be CGNC compatible).
Thus, we are using map_strg_gallus_ensp_symb/2 for the conversion.

==
?- edge_strg_gallus_symb( 'Lmtk3', Inter, W ).

==

@author nicos angelopoulos
@version  0.1 2022/12/19
*/
edge_strg_gallus_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( edge_strg_gallus_symb(X,Y,Z) ).

/**  map_strg_gallus_ensp_symb( Ensp, Symb ).

==
?- map_strg_gallus_ensp_symb( 'ENSGALP00000000003', Symb ).
Symb = 'RFK'.

==

Compare with: 
==
?- map_cgnc_gallus_cgnc_symb(Cgnc, 'RFK' ).
false.
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
map_strg_gallus_ensp_symb( X, Y ) :-
    bio_db:bio_db_serve( edge_strg_gallus_ensp_symb(X,Y) ).
