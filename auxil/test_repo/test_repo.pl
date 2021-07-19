
:- ensure_loaded(library(lib)).

:- lib(debug_call).

:- lib(bio_db).
:- lib(bio_db_repo).

:- debuc(test_repo).

/** test_repo.

    Test the current candidate for next pack(bio_db_repo). 

    Assumes it has been installed locally.
    Also make sure you installed the latest bio_db. The predicate tries to call each predicate imported from
    modules starting with _bio_db_hs_ or _bio_db_mouse_ and is prefixed by _map_ or _edge_.

@author nicos angelopoulos
@version  0.1 2017/10/14

*/
test_repo :-
    debuc( test_repo, start, true ),
    bio_db_repo_version( Version, Date ),
    debuc( test_repo, 'Version: ~w, published on: ~w', [Version,Date] ),
    test_repo_preds,
    debuc( test_repo, end, true ).

test_repo_preds :-
    predicate_property( Head, imported_from(From) ),
    ( atom_concat(bio_db_hs,_,From) ; atom_concat(bio_db_mouse,_,From) ),
    functor( Head, Pname, _Arity ),
    ( atom_concat(map,_,Pname) ; atom_concat(edge,_,Pname) ),
    debuc( test_repo, 'Head: ~w', [Head] ),
    once( Head ),
    debuc( test_repo, 'First: ~w', [Head] ),
    fail.
test_repo_preds.
