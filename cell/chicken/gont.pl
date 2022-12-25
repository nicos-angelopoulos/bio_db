:- module( bio_db_gallus_gont, [
                bio_db_gallus_gont/0,
                %       + String
                map_gont_gallus_symb_gont/4
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_gallus_gont.

Documentation predicate for chicken (gallus gallus) data from gene ontology data.

==
?- lib( &bio_db(gallus(gont)) ).
?- [ pack('bio_db/cell/gallus/gont') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_gallus_gont.

/**  map_gont_gallus_symb_gont( ?Symb, ?Gont ).

==
?- map_gont_gallus_symb_gont(S,G).

==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
map_gont_gallus_symb_gont( X, Y, Z, W ) :-
    bio_db:bio_db_serve( map_gont_gallus_symb_gont(X,Y,Z,W) ).
