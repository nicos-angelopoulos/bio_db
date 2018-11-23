:- module( bio_db_hs_ncbi, [
                bio_db_hs_ncbi/0,
                %       + NCBI
                map_ncbi_ensg_entz/2,
                map_ncbi_ensp_entz/2,
                map_ncbi_entz_ensg/2,
                map_ncbi_entz_ensp/2,
                map_ncbi_rnuc_symb/2,
                map_ncbi_dnuc_symb/2,
                map_ncbi_unig_entz/2
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_hs_ncbi.

Documentation predicate for Homo sapiens data from NCBI database.

==
?- lib( & bio_db(hs(ncbi)) ).
?- [ pack('bio_db/cell/hs/ncbi') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_ncbi.

/**  map_ncbi_ensp_entz( ?EnsP, ?Entz ).

Map predicate from Ensembl proteins to NCBI/entrez gene ids.

==
?- map_ncbi_ensp_entz( 'ENSP00000270238', Entz ).
Entz = 114783.
==
*/
map_ncbi_ensp_entz( X, Y ) :-
    bio_db:bio_db_serve( map_ncbi_ensp_entz(X,Y) ).

/**  map_ncbi_ensg_entz( ?EnsG, ?Entz ).

Map predicate from Ensembl genes to NCBI/entrez gene ids.

==
?- map_ncbi_ensg_entz( 'ENSG00000142235', Entz ).
Entz = 114783.
==
*/
map_ncbi_ensg_entz( X, Y ) :-
    bio_db:bio_db_serve( map_ncbi_ensg_entz(X,Y) ).

/**  map_ncbi_entz_ensp( ?Entz, ?EnsP ).

Map predicate from NCBI/entrez gene ids to Ensembl proteins.

==
?- map_ncbi_entz_ensp( 114783, EnsP ).
EnsP = 'ENSP00000270238'.

==
*/
map_ncbi_entz_ensp( X, Y ) :-
    bio_db:bio_db_serve( map_ncbi_entz_ensp(X,Y) ).

/** map_ncbi_rnuc_symb( RnaNucl, Symb ).

Map predicate from RNA nucleic sequence to HGNC symbol.

==
?- map_ncbi_rnuc_symb( 'BC140794', Symb ).
Symb = 'CEP170'.
==
*/
map_ncbi_rnuc_symb( Rnuc, Symb ) :-
    bio_db:bio_db_serve( map_ncbi_rnuc_symb(Rnuc,Symb) ).

/** map_ncbi_dnuc_symb( DnaNucl, Symb ).

Map predicate from DNA nucleic sequence to HGNC symbol.

==
?- map_ncbi_dnuc_symb( 'AL669831', Symb ).
Symb = 'CICP3' ;
...
Symb = 'TUBB8P11'.

==
*/
map_ncbi_dnuc_symb( Dnuc, Symb ) :-
    bio_db:bio_db_serve( map_ncbi_dnuc_symb(Dnuc,Symb) ).

/** map_ncbi_unig_entz( UniG, Entz ).

Map predicate from unigene to entrez id as per ncbi.

==
?- map_ncbi_unig_entz( 'Hs.80828', Entz ).
Entz = 3848.
==
*/
map_ncbi_unig_entz( UniG, Entz ) :-
    bio_db:bio_db_serve( map_ncbi_unig_entz(UniG,Entz) ).


/**  map_ncbi_entz_ensg( ?Entz, ?EnsG ).

Map predicate from NCBI/entrez gene ids to Ensembl genes.

==
?- map_ncbi_entz_ensg( 114783, EnsP ).
EnsP = 'ENSG00000142235'.
==
*/
map_ncbi_entz_ensg( X, Y ) :-
    bio_db:bio_db_serve( map_ncbi_entz_ensg(X,Y) ).

