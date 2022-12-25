:- module( bio_db_galg_gont, [
                bio_db_galg_gont/0,
                %       + String
                gont_galg_symb_gont/4
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_galg_gont.

Documentation predicate for chicken (gallus gallus) data from gene ontology data.

==
?- lib( &bio_db(galg(gont)) ).
?- [ pack('bio_db/cell/galg/gont') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_galg_gont.

/**  map_gont_galg_symb_gont( ?Symb, ?Gont ).

==
?- gont_galg_symb_gont(S,G).

==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
gont_galg_symb_gont( X, Y, Z, W ) :-
    bio_db:bio_db_serve( gont_galg_symb_gont(X,Y,Z,W) ).
