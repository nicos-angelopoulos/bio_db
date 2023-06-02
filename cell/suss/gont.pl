:- module( bio_db_suss_gont, [
                bio_db_suss_gont/0,
                %       + String
                gont_suss_symb_gont/4
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_suss_gont.

Documentation predicate for pig (sus scrofa) data from gene ontology data.

==
?- lib( &bio_db(suss(gont)) ).
?- [ pack('bio_db/cell/suss/gont') ].
==

@author nicos angelopoulos
@version  0.1 2023/6/2

*/
bio_db_suss_gont.

/**  gont_suss_symb_gont( ?Symb, ?Gont ).

==
?- gont_suss_symb_gont(S,G).

==

@author nicos angelopoulos
@version  0.1 2023/6/2

*/
gont_suss_symb_gont( X, Y, Z, W ) :-
    bio_db:bio_db_serve( gont_suss_symb_gont(X,Y,Z,W) ).
