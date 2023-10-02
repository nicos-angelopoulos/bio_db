:- module( bio_db_mult_vgnc, [
                bio_db_mult_vgnc/0,
                    %       + VGNC
                vgnc_mult_vgnc_name/3,   %  VGNC identifier, Taxonomy, and short descriptive name
                vgnc_mult_vgnc_symb/3    %  VGNC identifier, taxonomysymbol
                ] ).

:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_mult_vgnc.

Documentation predicate for multi-species data from VGNC database 
(selected Veterbrates Gene Name Committee).

Predicates defined:
  * vgnc_mult_vgnc_name/3
  * vgnc_mult_vgnc_symb/3

==
?- lib( & bio_db(mutl(vgnc)) ).
?- [pack('bio_db/cell/mult/vgnc')].
==

@author nicos angelopoulos
@version  0.1 2023/9/15
@see ncbi_mult_taxo_scnm/2, ncbi_mult_taxo_gbnm/2

*/
bio_db_mult_vgnc.

/** vgnc_mult_vgnc_symb(?Vgnc, ?Taxo, ?Symb).

Map predicate from VGNC unique integer identifier to unique gene symbol.

==
?- vgnc_homs_vgnc_symb(Vgnc, _, 'LMTK3').
VGNC = 30942;
fixme:
==

Compare to
==
?- hgnc_homs_hgnc_symb(Hgnc, 'LMTK3').
==

Get taxonomy, too
==
?- vgnc_homs_vgnc_symb(Vgnc, Taxo, 'LMTK3'), write(Vgnc:Taxo), nl, fail.
fixme:
==

*/
vgnc_mult_vgnc_symb( X, Y, Z ) :-
    bio_db:bio_db_serve( vgnc_mult_vgnc_symb(X,Y,Z) ).

/** vgnc_mult_vgnc_name(?Vgnc, ?Taxo, ?Name).

Map predicate from VGNC unique integer identifier to unique gene symbol.

==
?- vgnc_mult_vgnc_symb( Vgnc, 'LMTK3' ), vgnc_mult_vgnc_name(19295, Name).
Name = 'lemur tyrosine kinase 3'.
==

*/
vgnc_mult_vgnc_name( X, Y, Z ) :-
    bio_db:bio_db_serve( vgnc_mult_vgnc_name(X,Y,Z) ).

