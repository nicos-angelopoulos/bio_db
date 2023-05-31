:- module( bio_db_suss, [
                    bio_db_suss/0
                ] ).

:- use_module(library(lib)).

% :- lib( &(bio_db(suss(cgnc))) ).  % it doen't look like there is an equivelant for pig
:- lib( &(bio_db(suss(ense))) ).
:- lib( &(bio_db(suss(gont))) ).
:- lib( &(bio_db(suss(strg))) ).
:- lib( &(bio_db(suss(unip))) ).

/**  bio_db_suss.

Bio_db data sets for pig (sus scrofa)- token suss.

From chicken, fixme:
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
?- lib( & bio_db(suss) ).
?- lib( & bio_db(suss(ense)) ).
==

@author nicos angelopoulos
@version  0.1 2023/05/31

*/

bio_db_suss.
