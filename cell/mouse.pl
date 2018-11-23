:- module( bio_db_mouse, [
                    bio_db_mouse/0
                ] ).

:- use_module(library(lib)).

:- lib( &(bio_db(mouse(mgim))) ).
:- lib( &(bio_db(mouse(unip))) ).
:- lib( &(bio_db(mouse(gont))) ).
:- lib( &(bio_db(mouse(strg))) ).

/**  bio_db_mouse.

Bio_db data sets for mouse.

Databases
  * mgim
    MGI (Markers) 
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
@see 

*/

bio_db_mouse.
