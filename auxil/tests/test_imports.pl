
:- ensure_loaded(library(lists)).       % last/2, memberchk/2.
:- ensure_loaded(library(lib)).

:- lib(debug_call).

:- lib(bio_db).
:- lib(bio_db_repo).

:- debuc(test_imports).

/** test_imports.

    Test the current candidate for next pack(bio_db_repo). 

    Assumes bio_db and bio_db_repo have been installed locally.
    The predicate tries to call each predicate imported from bio_db starting with =|bio_db_Org_|=
    and where Org is in =|galg,homs,mult,musm,suss|=.

@author nicos angelopoulos
@version  0.1 2017/10/14
@version  0.2 2026/06/19,  updated with current organisms and pred naming; renamed from test_repo

*/
test_imports :-
    Self = test_imports,
    debuc( Self, start, true ),
    bio_db_repo_version( Version, Date ),
    debuc( Self, 'Version: ~w, published on: ~w', [Version,Date] ),
    test_import_preds( Self ),
    debuc( Self, end, true ).

test_import_preds(Self) :-
    predicate_property( Head, imported_from(From) ),
    atomic_list_concat( [bio,db,OrgT|_], '_', From ),
    memberchk( OrgT, [galg,homs,mult,musm,suss] ),
    functor( Head, Func, Arit ),   % exclude doc preds
    Arit =\= 0,
    atomic_list_concat( Parts, '_', Func ),
    last( Parts, Last ),
    \+ memberchk( Last, [edat] ),
    debuc( Self, 'Head: ~w', [Head] ),
    once( Head ),
    debuc( Self, 'First: ~w', [Head] ),
    fail.
test_import_preds(_Self).
