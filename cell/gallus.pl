:- module( bio_db_gallus, [
                    bio_db_gallus/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(gallus(cgnc))) ).
:- lib( &(bio_db(gallus(ense))) ).
:- lib( &(bio_db(gallus(gont))) ).
:- lib( &(bio_db(gallus(strg))) ).
:- lib( &(bio_db(gallus(unip))) ).

/**  bio_db_gallus.

Bio_db data sets for chicken (gallus gallus)- token gallus.

Please note that the identifiers in string are incompatible to the 
other databases on ensembl ids. That is why a map from ensembl proteins 
to symbols as per string is included (map_strg_gallus_ensp_symb/2).

Databases
  * cgnc
    chicken gene nomenclature committee
  * ense
    embl's ensembl
  * gont
    gene ontology
  * strg
    String PPIs db
  * unip
    Uniprot (all proteins)

==
?- lib( & bio_db(gallus) ).
?- lib( & bio_db(gallus(cgnc)) ).
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/

bio_db_gallus.
