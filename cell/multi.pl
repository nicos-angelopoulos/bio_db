:- module( bio_db_mult, [
                    bio_db_mult/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(mult(ncbi))) ).
:- lib( &(bio_db(mult(vgnc))) ).

/**  bio_db_mult.

Bio_db data sets for predicates that span multiple species token mult.

Databases
  * ncbi
    NCBI,  bio_db_mult_ncbi/0
  * vgnc
    VGNC, bio_db_vgnc_

==
?- lib( & bio_db(mult) ).
?- lib( & bio_db(mult(ncbi)) ).
?- lib( & bio_db(mult(vgnc)) ).
==

@author nicos angelopoulos
@version  0.1 2023/09/15
@tbd uniprot does not have protein files for pig similar to the ones for the other organisms
@tbd pig nonmeclature standard ? (similar to human HGNC or chicken CGNC)
@tbd ense also provides gont files
@see bio_db_mult_ncbi/0.
*/

bio_db_mult.
