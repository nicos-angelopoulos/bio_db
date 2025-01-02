:- module( bio_db_galg_reac, [
                    bio_db_galg_reac/0,
                    %       + Reactome
                    reac_galg_ncbi_reac/2,
                    reac_galg_ncbi_reap/3,
                    reac_galg_reac_reap/3,
                    reac_galg_reac_recl/2,
                    reac_galg_reac_recn/2,
                    reac_galg_reap_repn/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_galg_reac.

Documentation predicate for gallus gallus data from reactome database.

==
?- lib( &bio_db(galg(reac)) ).
?- [ pack('bio_db/cell/galg/reac') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/25

*/
bio_db_galg_reac.

/**  reac_galg_ncbi_reac( ?Ncbi, ?Reac ).

Map (chicken) NCBI identifiers to Reactome product ids.

==
?- reac_galg_ncbi_reac( 1000, Reac ).
==

*/
reac_galg_ncbi_reac( X, Y ) :-
    bio_db:bio_db_serve( reac_galg_ncbi_reac(X,Y) ).

/**  reac_galg_reac_reap( ?Reac, -Evidence, ?Reap ).

Map Reactome product identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_galg_reac_reap(191429, Evi, Reac), write(Evi-Reac), nl, fail.
==
*/
reac_galg_reac_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_galg_reac_reap(X,Y,Z) ).

/**  reac_galg_ncbi_reap( ?Ncbi, -Evidence, ?Reap ).

Map (chicken) NCBI identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_galg_ncbi_reap(1000, Evi, Reac), write(Evi-Reac), nl, fail.
==

*/
reac_galg_ncbi_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_galg_ncbi_reap(X,Y,Z) ).

/**  reac_galg_reac_recl( ?Reac, ?Recl ).

Map Reactome product identifiers to localisation tokens.

==
?- reac_galg_reac_recl( 191429, Recl ).
==
*/
reac_galg_reac_recl( X, Z ) :-
    bio_db:bio_db_serve( reac_galg_reac_recl(X,Z) ).

/**  reac_galg_reac_recn( ?Reac, ?Recn ).

Map Reactome identifiers to names.

==
?- reac_galg_reac_recn( Reac, Recn ).
==
*/
reac_galg_reac_recn( X, Z ) :-
    bio_db:bio_db_serve( reac_galg_reac_recn(X,Z) ).

/**  reac_galg_reap_repn( ?Reap, ?Repn ).

Map Reactome pathway identifiers to pathway names.

==
?- reac_galg_reap_repn( 525793, Repn ).
==
*/
reac_galg_reap_repn( X, Z ) :-
    bio_db:bio_db_serve( reac_galg_reap_repn(X,Z) ).
