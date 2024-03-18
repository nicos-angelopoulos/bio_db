:- module( bio_db_homs, [   bio_db_homs/0,
                            % symbols_string_graph/3,
                            hgnc_symbol/1
                        ] ).  

:- use_module(library(lib)).

:- lib( &(bio_db(homs(hgnc))) ).
:- lib( &(bio_db(homs(ense))) ).
:- lib( &(bio_db(homs(ncbi))) ).
:- lib( &(bio_db(homs(gont))) ).
:- lib( &(bio_db(homs(unip))) ).
:- lib( &(bio_db(homs(pros))) ).
:- lib( &(bio_db(homs(strg))) ).
:- lib( &(bio_db(homs(reac))) ).

%   4. derived
% :- lib( source(bio_db(homs)), homonyms(true) ).
% :- lib(symbols_string_graph/3).
% :- lib( end(bio_db(homs)) ).

/**  bio_db_homs.

Bio_db data sets for Homo sapiens. This cell is further
sub-divided according to the database the data come from.

Dbs
  * hgnc
    Hugo gene nomenclature 

  * ense
    ensemble genes: names and location

==
?- lib( & bio_db(human) ).
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2018/10/29


*/
bio_db_homs.

/** hgnc_symbol( ?Symbol ).

True iff Symbol is an HGNC symbol (deterministic for +Symbol).

==
?- bio_db:hgnc_symbol( 'LMTK3' ).
true.

==
*/
hgnc_symbol( Symbol ) :-
    ground( Symbol ),
    hgnc_homs_symb_hgnc( Symbol, _ ),
    !.
hgnc_symbol( Symbol ) :-
    hgnc_homs_symb_hgnc( Symbol, _ ).

