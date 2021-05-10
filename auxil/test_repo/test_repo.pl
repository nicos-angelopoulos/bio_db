
:- ensure_loaded(library(lib)).

:- lib(bio_db).
:- lib(debug_call).

:- debuc(test_repo).

/** test_repo.

    Test the current candidate for next bio_db_repo- from its downladed area, without installing proper first.

@author nicos angelopoulos
@version  0.1 2017/10/14

*/
test_repo :-
    debug_call( test_repo, start, true ),
    test_repo_preds,
    debug_call( test_repo, end, true ).

test_repo_preds :-
    predicate_property( Head, imported_from(bio_db) ),
    functor( Head, Pname, _Arity ),
    ( atom_concat(map,_,Pname) ; atom_concat(edge,_,Pname) ),
    debug( test_repo, 'Head: ~w', [Head] ),
    once( Head ),
    debug( test_repo, 'First: ~w', [Head] ),
    fail.
test_repo_preds.
