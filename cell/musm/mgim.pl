:- module( bio_db_musm_mgim, [
                bio_db_musm_mgim/0,
                %       + MGI database: fixme: url
                mgim_musm_mgim_chrl/5,
                mgim_musm_mgim_genb/2,
                mgim_musm_mgim_mrks/2,
                mgim_musm_mgim_ncbi/2,
                mgim_musm_mgim_unip/2,
                mgim_musm_mrks_wdra/2,
                mgim_musm_mgim_mnme/2,
                mgim_musm_msyn_mgim/2
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_musm_mgim.

Documentation predicate for mouse data from MGI database.
DNA fragments are referred to as markers in the database, thus
mgim is the 4 letter code for the database and its unique ids.

==
?- lib( &bio_db(musm(mgim)) ).
?- [pack('bio_db/cell/musm/mgim')].
==

@author nicos angelopoulos
@version  0.1 2018/11/3
@version  0.2 2022/12/25
@version  0.3 2023/10/1

*/
bio_db_musm_mgim.

/**  mgim_musm_mgim_chrl( +Mgim, -Chr, -Start, -End, -Strand ).

MGI marker to chromosomal location.

== 
?- mgim_musm_mgim_mrks(Mgim,'Lmtk3'), 
   mgim_musm_mgim_chrl( Mgim, Chr, Start, End, Strand ).

Mgim = 3039582,
Chr = 7,
Start = 45783738,
End = 45804144,
Strand =  (+).

==

*/
mgim_musm_mgim_chrl( Mgi, Chr, Sta, End, Sgn ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_chrl(Mgi,Chr,Sta,End,Sgn) ).

/**  mgim_musm_mgim_genb( +Mgim, -GenB ).

Map predicate from MGI marker to Gene Bank ids.

==
?- mgim_musm_mgim_mrks( Mgim, 'Lmtk3' ), 
   mgim_musm_mgim_genb( Mgim, GenB ), write( GenB ), nl, fail.

AB288873
AI413247
AK029204
AW046037
BC057032
BC059845
BC083185
BC094377
BE981921
false.
?-  
==
*/
mgim_musm_mgim_genb( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_genb(X,Y) ).

/**  mgim_musm_mgim_ncbi( +Mgim, -Ncbi ).

Map predicate from MGI marker to NCBI, Entrez ids.

==
?-  mgim_musm_mgim_mrks( Mgim, 'Lmtk3' ),
    mgim_musm_mgim_ncbi( Mgim, Ncbi ).

Ncbi = 381983,
Mgim = 3039582.

==
*/
mgim_musm_mgim_ncbi( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_ncbi(X,Y) ).

/**  mgim_musm_mgim_mrks( +Mgim, -Mrks ).

Map predicate between MGI identifier and MGI Marker Symbols.

MGI uses marker symbols for many different constructs.
This is a super-set of mgim_musm_mgim_symb/2, which constrains to Genes only.

*/
mgim_musm_mgim_mrks( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_mrks(X,Y) ).

/**  mgim_musm_mgim_symb( +Mgim, -mrks ).

Map predicate between MGI marker and (MGI) mrksols.

This is a sub-set of mgim_musm_mgim_mrks/2, as here we contraint to Genes only.

==
?- mgim_musm_mgim_symb( Mgim, 'Lmtk3' ).
Mgim = 3039582.
==
*/
mgim_musm_mgim_symb( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_symb(X,Y) ).

/** mgim_musm_mgim_mnme( X, Y ).

Map predicate between MGI marker mrksols and Marker names.

@tbd should this be replaced by mgim_mnme/2 ? 

*/
mgim_musm_mgim_mnme( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_mnme(X,Y) ).

/**  mgim_musm_mgim_unip( +Mgim, -Unip ).

Map predicate from MGI markder ids to Uniprot ids.

==
?- mgim_musm_mgim_mrks( Mgim, 'Lmtk3' ),
   mgim_musm_mgim_unip( Mgim, Unip ).

Mgim = 3039582,
Unip = 'Q5XJV6'.
==
*/
mgim_musm_mgim_unip( Sprt, Seqn ) :-
    bio_db:bio_db_serve( mgim_musm_mgim_unip(Sprt,Seqn) ).

/**  mgim_musm_mrks_wdra( +mrks, -Wdra ).

Map predicate from MGI mrksols to withdrawn names.

==
?- mgim_musm_mrks_wdra( S, W ).
S = 15006,
W = 'H2-Q1' ;
...
==
*/
mgim_musm_mrks_wdra( mrks, Wdra ) :-
    bio_db:bio_db_serve( mgim_musm_mrks_wdra(mrks,Wdra) ).

/*  mgim_musm_msyn_mgim( +Syno, -Mgim ).

Map predicate from Synonym to MGI marker ID.

==
?- mgim_musm_mgim_mrks( Mgim, 'Lmtk3' ),
|    mgim_musm_msyn_mgim( Syno, Mgim ).

Mgim = 3039582,
Syno = 'AATYK3' ;
Mgim = 3039582,
Syno = 'Aatyk3'.
==

*/
mgim_musm_msyn_mgim( X, Y ) :-
    bio_db:bio_db_serve( mgim_musm_msyn_mgim(X,Y) ).
