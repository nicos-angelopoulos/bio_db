:- module( bio_db_homs_pros, [
                bio_db_homs_pros/0,
                % Prosite
                pros_homs_pros_prsn/2,
                pros_homs_pros_sprt/7
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_pros.

Documentation predicate for Homo sapiens data from Uniprot database.

Predicates defined:
 * pros_homs_pros_prsn/2
 * pros_homs_pros_sprt/7

==
?- lib( & bio_db(homs(pros)) ).
?- [ pack('bio_db/cell/homs/pros') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/24

*/
bio_db_homs_pros.

/**  pros_homs_pros_prsn( +Pros, -Prsn ).

Map predicate: Prosite ID to Prosite Name.

==
==

*/
pros_homs_pros_prsn( X, Y ) :-
    bio_db:bio_db_serve( pros_homs_pros_prsn(X,Y) ).

/**  pros_homs_pros_sprt( +Pros, -Prsn, -Sprt, -Symb, -Start, -End, -Seqn ).

Map predicate from Prosite ID to (SwissProt) Protein info

==
==
*/
pros_homs_pros_sprt( P, N, S, B, T, E, Q ) :-
    bio_db:bio_db_serve( pros_homs_pros_sprt(P,N,S,B,T,E,Q) ).
