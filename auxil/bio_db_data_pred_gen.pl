
:- use_module(library(apply)).  % maplist/3.
:- use_module(library(lists)).  % member/2.

:- use_module(library(lib)).

:- lib(os_lib).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:date_two_digit_dotted/1).

/**  bio_db_data_pred_gen.

Generates bio_db_data_predicate/3 from the bio_db cell file sources.<br>
It assumes a 2-level tier structure in cell/. There is a pl file for each supported organism, with a number of mutually
exclusive cell files in the homonym sub directory.

For example the .pl files in cell/hs/ are the sub-parts comprising cell/hs.pl.

==
?- bio_db_data_pred_gen.
==

@author nicos angelopoulos
@version  0.1 2018/11/26

*/
bio_db_data_pred_gen :-
    tmp:ensure_loaded( '../pack.pl' ),
    once( tmp:version(Vers) ), 
    date_two_digit_dotted( Datted ),
    working_directory( Old, '../cell' ),
    os_files( TOses ),
    open( '../src/bio_db_data_predicate.pl', write, Out ),
    bio_db_data_pred_gen_prefix( Out ),
    maplist( bio_db_data_pred_gen_doc(Out), TOses, Pairs ),
    % bio_db_data_pred_gen_close_doc( Out ),
    write( Out, '%' ), nl( Out ),
    maplist( bio_db_data_pred_gen_clauses, Pairs, ClauseNest ),
    flatten( ClauseNest, Clauses ),
    write( Out, '% Generated on ' ), write( Out, Datted ), 
    write( Out, ' from bio_db with pack.pl version: ' ),
    write( Out, Vers ),
    write( Out, '.' ), nl( Out ),
    write( Out, '% ' ), nl( Out ),
    write( Out, '%' ), nl( Out ),
    portray_clauses( Clauses, stream(Out) ),
    close( Out ),
    working_directory( _, Old ).

bio_db_data_pred_gen_doc( Out, Os, Org-OrgPreds) :-
    write( doing(Out,Os,Org,OrgPreds) ), nl,
    os_ext( pl, Org, Os ),
    bio_db_data_org_token( Org, Okn ),
    os_files( Subs, dir(Okn) ),
    findall( SubF-Pred, ( member(Sub,Subs),
                      write( Out, '%  * '), write( Out, Org ),
                      write( Out, '/' ),
                      write( Out, Sub ), nl( Out ),
                      os_path(Okn,Sub,SubStem),
                      os_ext(pl,SubStem,SubF),
                      open(SubF,read,In),
                      read( In, (:- module(_,SubPreds)) ),
                      close(In),
                      member(Pred,SubPreds),
                      \+ Pred = _/0,
                      write( Out, '%    * '), write( Out, Pred ), nl( Out )
                    ),
                        Preds ),
    sort( Preds, OrgPreds ).

bio_db_data_pred_gen_clauses( Org-CPPairs, Clauses )  :-
    findall( bio_db_data_predicate(Pn,Pa,Org,Cell), member(Cell-Pn/Pa,CPPairs), Clauses ).

bio_db_data_pred_gen_prefix( Out ) :-
    write( Out, '%% bio_db_data_predicate( Pn, Pa, Org, Cell).' ), nl( Out ),
    write( Out, '%' ), nl( Out ),
    write( Out, '% Auto-generated predicate; listing and name-documenting all data predicates in bio_db.' ), nl( Out ),
    write( Out, '% This predicate is statically generated from the source cell files.' ), nl( Out ),
    write( Out, '%' ), nl( Out ).

bio_db_data_org_token( chicken, galg ).
bio_db_data_org_token( human, homs ).
bio_db_data_org_token( mouse, musm ).
