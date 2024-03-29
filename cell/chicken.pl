:- module( bio_db_galg, [
                    bio_db_galg/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(galg(cgnc))) ).
:- lib( &(bio_db(galg(ense))) ).
:- lib( &(bio_db(galg(gont))) ).
:- lib( &(bio_db(galg(ncbi))) ).
:- lib( &(bio_db(galg(reac))) ).
:- lib( &(bio_db(galg(strg))) ).
:- lib( &(bio_db(galg(unip))) ).

/**  bio_db_gallus.

Bio_db data sets for chicken (gallus gallus)- token galg.

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
  * ncbi
    NCBI (Entrez)
  * strg
    String PPIs db
  * unip
    Uniprot (all proteins)

==
?- lib( & bio_db(galg) ).
?- lib( & bio_db(galg(cgnc)) ).
==

@author nicos angelopoulos
@version  0.1 2022/12/19
@version  0.2 2024/03/07, db(ncbi)

*/

bio_db_galg.
