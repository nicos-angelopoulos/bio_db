
:- lib(&bio_db).

/**  lib_preds.

Print all predicates defined in bio_db module.

==
==

@author nicos angelopoulos
@version  0.1 2018/2/

*/
lib_preds :-
    lib_pred( Pred ),
    write( Pred ),  nl,
    fail.
lib_preds.

lib_pred( Pred-From ) :-
    predicate_property( Pred, imported_from(From) ),
    atom_concat( bio_db, _, From ).
