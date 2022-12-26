:- module( bio_db_homs_ncbi, [
                bio_db_homs_ncbi/0,
                %       + NCBI
                ncbi_homs_ensg_ncbi/2,
                ncbi_homs_ensp_ncbi/2,
                ncbi_homs_ncbi_ensg/2,
                ncbi_homs_ncbi_ensp/2,
                ncbi_homs_rnuc_symb/2,
                ncbi_homs_dnuc_symb/2
                % map_ncbi_unig_entz/2 % withdrawn 2019.02
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_ncbi.

Documentation predicate for Homo sapiens data from NCBI database.

==
?- lib( & bio_db(homs(ncbi)) ).
?- [ pack('bio_db/cell/homs/ncbi') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/25

*/
bio_db_homs_ncbi.

/**  ncbi_homs_ensp_entz( ?EnsP, ?Entz ).

Map predicate from Ensembl proteins to NCBI/entrez gene ids.

==
?- ncbi_homs_ensp_entz( 'ENSP00000270238', Entz ).
Entz = 114783.
==
*/
ncbi_homs_ensp_entz( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ensp_entz(X,Y) ).

/**  ncbi_homs_ensg_entz( ?EnsG, ?Entz ).

Map predicate from Ensembl genes to NCBI/entrez gene ids.

==
?- ncbi_homs_ensg_entz( 'ENSG00000142235', Entz ).
Entz = 114783.
==
*/
ncbi_homs_ensg_entz( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ensg_entz(X,Y) ).

/**  ncbi_homs_entz_ensp( ?Entz, ?EnsP ).

Map predicate from NCBI/entrez gene ids to Ensembl proteins.

==
?- ncbi_homs_entz_ensp( 114783, EnsP ).
EnsP = 'ENSP00000270238'.

==
*/
ncbi_homs_entz_ensp( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_entz_ensp(X,Y) ).

/** ncbi_homs_rnuc_symb( RnaNucl, Symb ).

Map predicate from RNA nucleic sequence to HGNC symbol.

==
?- ncbi_homs_rnuc_symb( 'BC140794', Symb ).
Symb = 'CEP170'.
==
*/
ncbi_homs_rnuc_symb( Rnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_homs_rnuc_symb(Rnuc,Symb) ).

/** ncbi_homs_dnuc_symb( DnaNucl, Symb ).

Map predicate from DNA nucleic sequence to HGNC symbol.

==
?- ncbi_homs_dnuc_symb( 'AL669831', Symb ).
Symb = 'CICP3' ;
...
Symb = 'TUBB8P11'.

==
*/
ncbi_homs_dnuc_symb( Dnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_homs_dnuc_symb(Dnuc,Symb) ).


/* map_ncbi_unig_entz( UniG, Entz ).

UNIGENE WAS WITHDRAWN ON FEB 2019.

Map predicate from unigene to entrez id as per ncbi.

==
?- map_ncbi_unig_entz( 'Hs.80828', Entz ).
Entz = 3848.
==
map_ncbi_unig_entz( UniG, Entz ) :-
    bio_db:bio_db_serve( map_ncbi_unig_entz(UniG,Entz) ).
*/

/** ncbi_homs_entz_ensg( ?Entz, ?EnsG ).

Map predicate from NCBI/entrez gene ids to Ensembl genes.

==
?- ncbi_homs_entz_ensg( 114783, EnsP ).
EnsP = 'ENSG00000142235'.
==
*/
ncbi_homs_entz_ensg( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_entz_ensg(X,Y) ).

