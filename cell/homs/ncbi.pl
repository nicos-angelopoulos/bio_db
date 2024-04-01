:- module( bio_db_homs_ncbi, [
                bio_db_homs_ncbi/0,
                %       + NCBI
                ncbi_homs_dnuc_symb/2,
                % ncbi_homs_ensg_ncbi/2,
                % ncbi_homs_ensp_ncbi/2,
                ncbi_homs_ncbi_ensg/2,
                ncbi_homs_ncbi_ensp/2,
                ncbi_homs_ncbi_symb/2,
                ncbi_homs_nsyn_symb/2,
                ncbi_homs_rnuc_symb/2 
                % map_ncbi_unig_ncbi/2 % withdrawn 2019.02
                ] ).
                
:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_ncbi.

Documentation predicate for Homo sapiens data from NCBI database.

Predicates defined:
  * ncbi_homs_dnuc_symb/2
  * ncbi_homs_ncbi_ensg/2
  * ncbi_homs_ncbi_ensp/2
  * ncbi_homs_ncbi_symb/2
  * ncbi_homs_nsyn_symb/2
  * ncbi_homs_rnuc_symb/2

==
?- lib( & bio_db(homs(ncbi)) ).
?- [ pack('bio_db/cell/homs/ncbi') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/25

*/
bio_db_homs_ncbi.

/**  ncbi_homs_ensp_ncbi( ?EnsP, ?Ncbi ).

Map predicate from Ensembl proteins to NCBI/entrez gene ids.

==
?- ncbi_homs_ensp_ncbi( 'ENSP00000270238', Ncbi ).
Ncbi = 114783.
==
*/
ncbi_homs_ensp_ncbi( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ensp_ncbi(X,Y) ).

/**  ncbi_homs_ensg_ncbi( ?EnsG, ?Ncbi ).

Map predicate from Ensembl genes to NCBI/entrez gene ids.

==
?- ncbi_homs_ensg_ncbi( 'ENSG00000142235', Ncbi ).
Ncbi = 114783.
==
*/
ncbi_homs_ensg_ncbi( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ensg_ncbi(X,Y) ).

/**  ncbi_homs_ncbi_ensp( ?Ncbi, ?EnsP ).

Map predicate from NCBI/entrez gene ids to Ensembl proteins.

==
?- ncbi_homs_ncbi_ensp( 114783, EnsP ).
EnsP = 'ENSP00000270238'.

==
*/
ncbi_homs_ncbi_ensp( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ncbi_ensp(X,Y) ).

/** ncbi_homs_dnuc_symb( DnaNucl, Symb ).

Map predicate from DNA nucleic sequence to NCBI symbol.

==
?- ncbi_homs_dnuc_symb( 'AL669831', Symb ).
Symb = 'CICP3' ;
...
Symb = 'TUBB8P11'.

==
*/
ncbi_homs_dnuc_symb( Dnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_homs_dnuc_symb(Dnuc,Symb) ).


/* map_ncbi_unig_ncbi( UniG, Ncbi ).

UNIGENE WAS WITHDRAWN ON FEB 2019.

Map predicate from unigene to entrez id as per ncbi.

==
?- map_ncbi_unig_ncbi( 'Hs.80828', Ncbi ).
Ncbi = 3848.
==
map_ncbi_unig_ncbi( UniG, Ncbi ) :-
    bio_db:bio_db_serve( map_ncbi_unig_ncbi(UniG,Ncbi) ).
*/

/** ncbi_homs_ncbi_ensg( ?Ncbi, ?EnsG ).

Map predicate from NCBI/entrez gene ids to Ensembl genes.

==
?- ncbi_homs_ncbi_ensg( 114783, EnsP ).
EnsP = 'ENSG00000142235'.
==
*/
ncbi_homs_ncbi_ensg( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ncbi_ensg(X,Y) ).

/** ncbi_homs_ncbi_symb( ?Ncbi, ?Symb).

Map predicate from NCBI/entrez gene ids to Symbols. 

Note that the Symbols are no checked against HGNC. They are what NCBI calls symbols.

==
?- ncbi_homs_ncbi_symb( 114783, Symb ).
Symb = 'LMTK3'.
==
*/
ncbi_homs_ncbi_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_ncbi_symb(X,Y) ).

/** ncbi_homs_nsyn_symb( ?Nsyn, ?Symb).

Map predicate (NCBI) synonym to (NCBI) Symbol.

==
?- ncbi_homs_nsyn_symb( Syno, 'LMTK3' ).
Syno = 'LMR3' ;
Syno = 'PPP1R101' ;
Syno = 'TYKLM3'.
==
*/
ncbi_homs_nsyn_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_homs_nsyn_symb(X,Y) ).

/** ncbi_homs_rnuc_symb( RnaNucl, Symb ).

Map predicate from RNA nucleic sequence to HGNC symbol.

==
?- ncbi_homs_rnuc_symb( 'BC140794', Symb ).
Symb = 'CEP170'.
==
*/
ncbi_homs_rnuc_symb( Rnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_homs_rnuc_symb(Rnuc,Symb) ).
