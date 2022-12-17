:- module( bio_db_mouse, [
                    bio_db_mouse/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(gallus(cgnc))) ).
:- lib( &(bio_db(gallus(gont))) ).

/**  bio_db_mouse.

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
?- lib( & bio_db(mouse) ).
?- lib( & bio_db(mouse(mgim)) ).
==

@author nicos angelopoulos
@version  0.1 2018/10/31
@version  0.2 2019/2/11
@see 

*/

bio_db_mouse.
