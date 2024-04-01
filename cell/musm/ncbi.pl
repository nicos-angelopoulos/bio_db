:- module( bio_db_musm_ncbi, [
                bio_db_musm_ncbi/0,
                %       (mouse) NCBI (entrez synonyms only, for now)
                ncbi_musm_dnuc_symb/2,
                ncbi_musm_ncbi_ensg/2,
                ncbi_musm_ncbi_ensp/2,
                ncbi_musm_ncbi_symb/2,
                ncbi_musm_nsyn_symb/2,
                ncbi_musm_rnuc_symb/2 
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_musm_ncbi.

Documentation predicate for mouse data from NCBI database.

Predicates defined:
 * ncbi_musm_dnuc_symb/2
 * ncbi_musm_ncbi_ensg/2
 * ncbi_musm_ncbi_ensp/2
 * ncbi_musm_ncbi_symb/2
 * ncbi_musm_nsyn_symb/2
 * ncbi_musm_rnuc_symb/2 

==
?- lib( &bio_db(musm(ncbi)) ).
?- [ pack('bio_db/cell/musm/ncbi') ].

==

@author nicos angelopoulos
@version  0.1 2019/2/12
@version  0.2 2022/12/25

*/
bio_db_musm_ncbi.

/**  ncbi_musm_dnuc_symb(+Dnuc, -Symb).

Map predicate from DNA nucleic sequence to NCBI symbol.

== 
?- ncbi_musm_dnuc_symb(D,S).
==

*/
ncbi_musm_dnuc_symb( Dnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_musm_dnuc_symb(Dnuc,Symb) ).

/** ncbi_musm_ncbi_ensg( ?Ncbi, ?EnsG ).

Map predicate from NCBI/entrez gene ids to Ensembl genes.

==
?- ncbi_musm_ncbi_ensg( Ncbi, EnsG ).
==
*/
ncbi_musm_ncbi_ensg( X, Y ) :-
    bio_db:bio_db_serve( ncbi_musm_ncbi_ensg(X,Y) ).

/**  ncbi_musm_ncbi_ensp( ?Ncbi, ?EnsP ).

Map predicate from NCBI/entrez gene ids to Ensembl proteins.

==
?- ncbi_musm_ncbi_ensp( 114783, EnsP ).
EnsP = 'ENSP00000270238'.

==
*/
ncbi_musm_ncbi_ensp( X, Y ) :-
    bio_db:bio_db_serve( ncbi_musm_ncbi_ensp(X,Y) ).

/** ncbi_musm_ncbi_symb( ?Ncbi, ?Symb).

Map predicate from NCBI/entrez gene ids to Symbols. 

Note that the Symbols are no checked against HGNC. They are what NCBI calls symbols.

==
?- ncbi_musm_ncbi_symb( Ncbi, Symb ).
==
*/
ncbi_musm_ncbi_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_musm_ncbi_symb(X,Y) ).

/**  ncbi_musm_nsyn_symb( +Syno, -Symb ).

Map of symbol synonyms based on the NCBI (Entrez) data.

== 
?- ncbi_musm_nsyn_symb( 'Znf638', Symb ).
Symb = 'Zfp638'.

?- ncbi_musm_nsyn_symb( 'Stx5', Symb ).
Symb = 'Stx5a'.
==

*/
ncbi_musm_nsyn_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_musm_nsyn_symb(X,Y) ).

/** ncbi_musm_rnuc_symb( RnaNucl, Symb ).

Map predicate from RNA nucleic sequence to HGNC symbol.

==
?- ncbi_musm_rnuc_symb( Rnuc, Symb ).
==
*/
ncbi_musm_rnuc_symb( Rnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_musm_rnuc_symb(Rnuc,Symb) ).
