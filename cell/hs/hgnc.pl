:- module( bio_db_hs_hgnc, [
                bio_db_hs_hgnc/0,
                    %       + HGNC
                map_hgnc_ccds_hgnc/2, % concesus protein coding regions
                map_hgnc_ensg_hgnc/2, %
                map_hgnc_entz_hgnc/2, %  
                map_hgnc_entz_symb/2, %
                map_hgnc_hgnc_ccds/2, % concesus protein coding regions
                map_hgnc_hgnc_chrb/2,
                map_hgnc_hgnc_ensg/2, %
                map_hgnc_hgnc_entz/2,
                map_hgnc_hgnc_name/2, %
                map_hgnc_hgnc_symb/2,
                map_hgnc_prev_symb/2, %
                map_hgnc_symb_entz/2, %
                map_hgnc_symb_hgnc/2, %
                map_hgnc_syno_symb/2  %
                % removed from the source files, so removed from here 19.02.08
                % 'map_hgnc_entz-appv_symb'/2,
                % 'map_hgnc_entz-ncbi_symb'/2,
                % 'map_hgnc_hgnc_entz-appv'/2,
                % 'map_hgnc_hgnc_entz-ncbi'/2
                ] ).

:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_hs_hgnc.

Documentation predicate for Homo sapiens data from HGNC database.

==
?- lib( & bio_db(hs(hgnc)) ).
?- [ pack('bio_db/cell/hs/hgnc') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/22

*/
bio_db_hs_hgnc.

/**  map_hgnc_hgnc_symb( ?Hgnc, ?Symb ).

Map predicate from HGNC unique integer identifier to unique gene symbol.

==
?- map_hgnc_hgnc_symb( 19295, Symb ).
Symb = 'LMTK3'.
==

*/
map_hgnc_hgnc_symb( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_symb(X,Y) ).

/**  map_hgnc_hgnc_name( ?Hgnc, ?Symb ).

Map predicate from HGNC unique integer identifier to unique gene name/description.

==
?- map_hgnc_hgnc_name( 19295, Name ).
Name = 'lemur tyrosine kinase 3'.
==

*/
map_hgnc_hgnc_name( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_name(X,Y) ).

/**  map_hgnc_symb_hgnc( ?Symb, ?Hgnc ).

Map predicate from HGNC unique symbol to unique HGNC integer identifier.

==
?- map_hgnc_symb_hgnc( 'LMTK3', HGNC ).
HGNC = 19295.
==
*/
map_hgnc_symb_hgnc( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_symb_hgnc(X,Y) ).

/**  map_hgnc_syno_symb( ?Syno, ?Symb).

Map predicate from gene synonyms to approved HGNC Symbol.

==
?- map_hgnc_syno_symb( 'LMR3', Symb ).
Symb = 'LMTK3'.
==
*/
map_hgnc_syno_symb( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_syno_symb(X,Y) ).

/**  map_hgnc_prev_symb( ?Prev, ?Symb ).

Map predicate from previously known-as gene names to approved HGNC Symbol.

==
?- map_hgnc_prev_symb( 'ERBB', Symb ).
Symb = 'EGFR'.
==
*/
map_hgnc_prev_symb( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_prev_symb(X,Y) ).

/**  map_hgnc_ccds_hgnc( ?Ccds, ?Hgnc ).

Map predicate from concesus protein coding regions to HGNC ID.

==
?- map_hgnc_ccds_hgnc( 'CCDS11576', Hgnc ).
Hgnc = 11979.

==
*/
map_hgnc_ccds_hgnc( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_ccds_hgnc(X,Y) ).

/**  map_hgnc_hgnc_ccds( ?Hgnc, ?Ccds ).

Map predicate from HGNC ID to concesus protein coding regions.

==
?- map_hgnc_hgnc_ccds( 11979,  Ccds ).
Ccds = 'CCDS11576'.

==
*/
map_hgnc_hgnc_ccds( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_ccds(X,Y) ).

/**  map_hgnc_ensg_hgnc( ?Ensg, ?Symb ).

Map predicate from Ensembl gene id to HGNC Id.

==
?- map_hgnc_ensg_hgnc( Ensg, 19295 ).
Ensg = 'ENSG00000142235'.
==
*/
map_hgnc_ensg_hgnc( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_ensg_hgnc(X,Y) ).

/**  map_hgnc_symb_entz( ?Symb, ?Entz ).

Map predicate from HGNC symbols to (NCBI) entrez gene ids.

==
?- map_hgnc_symb_entz( 'LMTK3', Etnz ).
Etnz = 114783.
==
*/
map_hgnc_symb_entz( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_symb_entz(X,Y) ).

/**  map_hgnc_entz_hgnc( ?Entz, ?Symb ).

Map predicate from entrez ids to approved HGNC Symbol.

==
?- map_hgnc_entz_hgnc( 114783, Symb ).
Symb = 19295.
==
*/
map_hgnc_entz_hgnc( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_entz_hgnc(X,Y) ).


/**  map_hgnc_entz_symb( ?Entz, ?Symb ).

Map predicate from entrez ids to approved HGNC Symbol.
==
?- map_hgnc_entz_symb( 114783, Symb ).
Symb = 'LMTK3'.
==
*/
map_hgnc_entz_symb( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_entz_symb(X,Y) ).

/**  map_hgnc_hgnc_chrb( +Hgnc, -ChrB ).

Map predicate from HGNC ID to Chromosome Band 
==
?- map_hgnc_hgnc_chrb( 5, ChrB ).
ChrB = '19q13.43'
==
*/
map_hgnc_hgnc_chrb( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_chrb(X,Y) ).

/**  map_hgnc_hgnc_ensg( +Hgnc, -EnsG ).

Map predicate from HGNC ID to Ensembl Gene
==
==
*/
map_hgnc_hgnc_ensg( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_ensg(X,Y) ).

/**  map_hgnc_hgnc_entz( +Hgnc, -Entz ).

Map predicate from HGNC ID to Ensembl Gene (by all means available)
==
==
*/
map_hgnc_hgnc_entz( X, Y ) :-
    bio_db:bio_db_serve( map_hgnc_hgnc_entz(X,Y) ).


