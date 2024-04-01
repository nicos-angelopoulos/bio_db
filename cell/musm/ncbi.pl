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

==
?- lib( &bio_db(musm(ncbi)) ).
?- [ pack('bio_db/cell/musm/ncbi') ].

==

@author nicos angelopoulos
@version  0.1 2019/2/12
@version  0.2 2022/12/25

*/
bio_db_musm_ncbi.

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
