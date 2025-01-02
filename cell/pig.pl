:- module( bio_db_suss, [
                    bio_db_suss/0
                ] ).

:- use_module(library(lib)).

% :- lib( &(bio_db(suss(cgnc))) ).  % it doen't look like there is an equivelant for pig
:- lib( &(bio_db(suss(ense))) ).
:- lib( &(bio_db(suss(gont))) ).
:- lib( &(bio_db(suss(ncbi))) ).
:- lib( &(bio_db(suss(reac))) ).
:- lib( &(bio_db(suss(strg))) ).
:- lib( &(bio_db(suss(vgnc))) ).
% :- lib( &(bio_db(suss(unip))) ).

/**  bio_db_suss.

Bio_db data sets for pig (sus scrofa)- token suss.

Databases
  * ense
    embl's ensembl, bio_db_suss_ense/0
  * gont
    gene ontology, bio_db_suss_gont/0
  * ncbi
    NCBI ids (previously Entrez ids), bio_db_suss_ncbi/0
  * strg
    String PPIs db, bio_db_suss_strg/0

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
@see bio_db_suss_ense/0, bio_db_suss_gont/0, bio_db_suss_ncbi/0, bio_db_suss_strg/0, bio_db_suss_vgnc/0.
*/

bio_db_suss.
