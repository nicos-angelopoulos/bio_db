:- module( bio_db_homs_reac, [
                    bio_db_homs_reac/0,
                    %       + Reactome
                    reac_homs_ncbi_reac/2,
                    reac_homs_ncbi_reap/3,
                    reac_homs_reac_reap/3,
                    reac_homs_reac_recl/2,
                    reac_homs_reac_recn/2,
                    reac_homs_reap_repn/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_reac.

Documentation predicate for Homo sapiens data from reactome database.

Predicatess defined:
  * reac_homs_ncbi_reac/2
  * reac_homs_ncbi_reap/2
  * reac_homs_reac_reap/3
  * reac_homs_reac_recl/2
  * reac_homs_reac_recn/2
  * reac_homs_reap_repn/2

==
?- lib( &bio_db(homs(reac)) ).
?- [ pack('bio_db/cell/homs/reac') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/24

*/
bio_db_homs_reac.

/**  reac_homs_ncbi_reac( ?Ncbi, ?Reac ).

Map (human) NCBI identifiers to Reactome product ids.

==
?- reac_homs_ncbi_reac( 1000, Reac ).
Reac = 191429 ;
Reac = 8956761 ;
Reac = 8956899.
==

*/
reac_homs_ncbi_reac( X, Y ) :-
    bio_db:bio_db_serve( reac_homs_ncbi_reac(X,Y) ).

/**  reac_homs_reac_reap( ?Reac, -Evidence, ?Reap ).

Map Reactome product identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_homs_reac_reap(191429, Evi, Reac), write(Evi-Reac), nl, fail.
IEA-525793
IEA-1266738
TAS-418990
...
==
*/
reac_homs_reac_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_homs_reac_reap(X,Y,Z) ).

/**  reac_homs_ncbi_reap( ?Ncbi, -Evidence, ?Reap ).

Map (human) NCBI identifiers to Reactome pathway ids.

Evidence is the type of evidence supporting the pathway membership.

==
?- reac_homs_ncbi_reap(1000, Evi, Reac), write(Evi-Reac), nl, fail.
IEA-525793
IEA-1266738
TAS-381426
TAS-392499
TAS-418990
...
==

==
?- 
     findall(Ncbi, (reac_homs_reap_repn(Reap,'Signal Transduction'),reac_homs_ncbi_reap(Ncbi,_,Reap)),Ncbis),length(Ncbis,Len).

Ncbis = [14, 19, 25, 27, 29, 59, 60, 60, 70|...],
Len = 3215.
==

*/
reac_homs_ncbi_reap( X, Y, Z ) :-
    bio_db:bio_db_serve( reac_homs_ncbi_reap(X,Y,Z) ).

/**  reac_homs_reac_recl( ?Reac, ?Recl ).

Map Reactome product identifiers to localisation tokens.

==
?- reac_homs_reac_recl( 191429, Recl ).
Recl = 'plasma membrane'.
==
*/
reac_homs_reac_recl( X, Z ) :-
    bio_db:bio_db_serve( reac_homs_reac_recl(X,Z) ).

/**  reac_homs_reac_recn( ?Reac, ?Recn ).

Map Reactome product identifiers to product names.

==
?- reac_homs_reac_recn( 191429, Recn ).
Recn = 'CDH2'.
==
*/
reac_homs_reac_recn( X, Z ) :-
    bio_db:bio_db_serve( reac_homs_reac_recn(X,Z) ).

/**  reac_homs_reap_repn( ?Reap, ?Repn ).

Map Reactome pathway identifiers to pathway names.

==
?- reac_homs_reap_repn( 525793, Repn ).
Repn = 'Myogenesis'.
==
*/
reac_homs_reap_repn( X, Z ) :-
    bio_db:bio_db_serve( reac_homs_reap_repn(X,Z) ).

