:- module( bio_db_hs, [bio_db_hs/0,
                % derived preds:
                go_term_symbols/3,
                symbols_string_graph/3,
                hgnc_symbol/1
                        ] ).  

:- use_module(library(lib)).

:- lib( &(bio_db(hs(hgnc))) ).
:- lib( &(bio_db(hs(ense))) ).
:- lib( &(bio_db(hs(ncbi))) ).
:- lib( &(bio_db(hs(gont))) ).
:- lib( &(bio_db(hs(unip))) ).
:- lib( &(bio_db(hs(pros))) ).
:- lib( &(bio_db(hs(strg))) ).

%   4. derived
:- lib( source(bio_db(hs)), homonyms(true) ).
:- lib(go_term_symbols/3).
:- lib(symbols_string_graph/3).
:- lib( end(bio_db(hs)) ).

/**  bio_db_hs.

Bio_db data sets for Homo sapiens. This cell is further
sub-divided according to the database the data come from.

Dbs
  * hgnc
    Hugo gene nomenclature 

  * ense
    ensemble genes: names and location

==
?- lib( & bio_db(hs) ).
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs.

/** hgnc_symbol( ?Symbol ).

True iff Symbol is an HGNC symbol (deterministic for +Symbol).

==
?- bio_db:hgnc_symbol( 'LMTK3' ).
true.

==
*/
hgnc_symbol( Symbol ) :-
    ground( Symbol ),
    map_hgnc_symb_hgnc( Symbol, _ ),
    !.
hgnc_symbol( Symbol ) :-
    map_hgnc_symb_hgnc( Symbol, _ ).

