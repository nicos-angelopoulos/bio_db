:- module( bio_db_musm_reac, [
                    bio_db_musm_reac/0,
                    %       + Reactome
                    reac_musm_ncbi_reac/2,
                    reac_musm_ncbi_reap/2,
                    reac_musm_reac_reap/3,
                    reac_musm_reac_recl/2,
                    reac_musm_reac_recn/2,
                    reac_musm_reap_repn/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_musm_reac.

Documentation predicate for Homo sapiens data from reactome database.

==
?- lib( &bio_db(musm(reac)) ).
?- [ pack('bio_db/cell/musm/reac') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/25

*/
bio_db_musm_reac.

/**  reac_musm_ncbi_reac( ?Ncbi, ?Reac ).

Map (mouse) NCBI identifiers to Reactome product ids.

==
?- reac_musm_ncbi_reac( 1000, Reac ).
==

*/
reac_musm_ncbi_reac( X, Y ) :-
    bio_db:bio_db_serve( reac_musm_ncbi_reac(X,Y) ).

/**  reac_musm_reac_reap( ?Reac, -Evidence, ?Reap ).

Map Reactome product identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_musm_reac_reap(191429, Evi, Reac), write(Evi-Reac), nl, fail.
==
*/
reac_musm_ncbi_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_musm_reac_reap(X,Y,Z) ).

/**  reac_musm_ncbi_reap( ?Ncbi, -Evidence, ?Reap ).

Map (mouse) NCBI identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_musm_ncbi_reap(1000, Evi, Reac), write(Evi-Reac), nl, fail.
==

*/
reac_musm_ncbi_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_musm_ncbi_reap(X,Y,Z) ).

/**  reac_musm_reac_recl( ?Reac, ?Recl ).

Map Reactome product identifiers to localisation tokens.

==
?- reac_musm_reac_recl( 191429, Recl ).
==
*/
reac_musm_reac_recl( X, Z ) :-
    bio_db:bio_db_serve( reac_musm_reac_recl(X,Z) ).

/**  reac_musm_reac_recn( ?Reac, ?Recn ).

Map Reactome product identifiers to product names.

==
?- reac_musm_reac_recn( 191429, Recn ).
==
*/
reac_musm_reac_recn( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_musm_reac_recn(X,Y,Z) ).

/**  reac_musm_reap_repn( ?Reap, ?Repn ).

Map Reactome pathway identifiers to pathway names.

==
?- reac_musm_reap_repn( 525793, Repn ).
==
*/
reac_musm_reap_repn( X, Z ) :-
    bio_db:bio_db_serve( reac_musm_reap_repn(X,Z) ).
