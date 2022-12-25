:- module( bio_db_hs_pros, [
                bio_db_hs_pros/0,
                % Prosite
                map_pros_pros_prsn/2,
                map_pros_pros_sprt/7
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_hs_pros.

Documentation predicate for Homo sapiens data from Uniprot database.

==
?- lib( & bio_db(hs(pros)) ).
?- [ pack('bio_db/cell/hs/pros') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_pros.

/**  map_pros_pros_prsn( +Pros, -Prsn ).

Map predicate: Prosite ID to Prosite Name.

==
==

*/
map_pros_pros_prsn( X, Y ) :-
    bio_db:bio_db_serve( map_pros_pros_prsn(X,Y) ).

/**  map_pros_pros_sprt( +Pros, -Prsn, -Sprt, -Symb, -Start, -End, -Seqn ).

Map predicate from Prosite ID to (SwissProt) Protein info

==
==
*/
map_pros_pros_sprt( P, N, S, B, T, E, Q ) :-
    bio_db:bio_db_serve( map_pros_pros_sprt(P,N,S,B,T,E,Q) ).
