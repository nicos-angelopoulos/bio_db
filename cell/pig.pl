:- module( bio_db_suss, [
                    bio_db_suss/0
                ] ).

:- use_module(library(lib)).

% :- lib( &(bio_db(suss(cgnc))) ).  % it doen't look like there is an equivelant for pig
:- lib( &(bio_db(suss(ense))) ).
:- lib( &(bio_db(suss(gont))) ).
:- lib( &(bio_db(suss(strg))) ).
% :- lib( &(bio_db(suss(unip))) ).

/**  bio_db_suss.

Bio_db data sets for pig (sus scrofa)- token suss.

Databases
  * ense
    embl's ensembl
  * gont
    gene ontology
  * ncbi
    NCBI ids (previously Entrez ids)
  * strg
    String PPIs db

==
?- lib( & bio_db(suss) ).
?- lib( & bio_db(suss(ense)) ).
?- lib( & bio_db(suss(gont)) ).
?- lib( & bio_db(suss(ncbi)) ).
?- lib( & bio_db(suss(strg)) ).
==

@author nicos angelopoulos
@version  0.1 2023/06/02
@tbd uniprot does not have protein files for pig similar to the ones for the other organisms
@tbd pig nonmeclature standard ? (similar to human HGNC or chicken CGNC)
@tbd ense also provides gont files

*/

bio_db_suss.
