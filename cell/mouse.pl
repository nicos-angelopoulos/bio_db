:- module( bio_db_musm, [
                    bio_db_musm/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(musm(mgim))) ).
:- lib( &(bio_db(musm(ense))) ).
:- lib( &(bio_db(musm(ncbi))) ).
:- lib( &(bio_db(musm(unip))) ).
:- lib( &(bio_db(musm(reac))) ).
:- lib( &(bio_db(musm(gont))) ).
:- lib( &(bio_db(musm(strg))) ).

/**  bio_db_musm.

Bio_db data sets for mouse.

Databases
  * mgim
    MGI (Markers), the main db for mouse gene identifiers and symbols
  * ense
    embl's ensembl
  * ncbi
    entrez synonyms (entrez ids are taken from MGI)
  * unip
    (mouse) Swiss Prot and Trembl
  * strg
    String PPIs db

==
?- lib( & bio_db(musm) ).
?- lib( & bio_db(musm(mgim)) ).
==

@author nicos angelopoulos
@version  0.1 2018/10/31
@version  0.2 2019/2/11
@version  0.3 2022/12/25

*/

bio_db_musm.
