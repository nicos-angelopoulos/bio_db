:- module( bio_db_homs_hgnc, [
                bio_db_homs_hgnc/0,
                    %       + HGNC
                hgnc_homs_ccds_hgnc/2, % concesus protein coding regions
                hgnc_homs_ensg_hgnc/2, %
                hgnc_homs_ncbi_hgnc/2, %  
                hgnc_homs_ncbi_symb/2, %
                hgnc_homs_hgnc_ccds/2, % concesus protein coding regions
                hgnc_homs_hgnc_chrb/2,
                hgnc_homs_hgnc_ensg/2, %
                hgnc_homs_hgnc_ncbi/2,
                hgnc_homs_hgnc_name/2, %
                hgnc_homs_hgnc_symb/2,
                hgnc_homs_prev_symb/2, %
                hgnc_homs_symb_ncbi/2, %
                hgnc_homs_symb_hgnc/2, %
                hgnc_homs_syno_symb/2  %
                % removed from the source files, so removed from here 19.02.08
                % 'map_hgnc_ncbi-appv_symb'/2,
                % 'map_hgnc_ncbi-ncbi_symb'/2,
                % 'map_hgnc_hgnc_ncbi-appv'/2,
                % 'map_hgnc_hgnc_ncbi-ncbi'/2
                ] ).

:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_homs_hgnc.

Documentation predicate for Homo sapiens data from HGNC database.

Predicates defined:
  * hgnc_homs_ccds_hgnc/2
  * hgnc_homs_ensg_hgnc/2
  * hgnc_homs_ncbi_hgnc/2
  * hgnc_homs_ncbi_symb/2
  * hgnc_homs_hgnc_ccds/2
  * hgnc_homs_hgnc_chrb/2
  * hgnc_homs_hgnc_ensg/2
  * hgnc_homs_hgnc_ncbi/2
  * hgnc_homs_hgnc_name/2
  * hgnc_homs_hgnc_symb/2
  * hgnc_homs_prev_symb/2
  * hgnc_homs_symb_ncbi/2
  * hgnc_homs_symb_hgnc/2
  * hgnc_homs_syno_symb/2

==
?- lib( & bio_db(homs(hgnc)) ).
?- [ pack('bio_db/cell/homs/hgnc') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/22
@version  0.2 2022/12/25

*/
bio_db_homs_hgnc.

/** hgnc_homs_hgnc_symb(?Hgnc, ?Symb).

Map predicate from HGNC unique integer identifier to unique gene symbol.

==
?- hgnc_homs_hgnc_symb(19295, Symb).
Symb = 'LMTK3'.
==

*/
hgnc_homs_hgnc_symb( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_symb(X,Y) ).

/**  hgnc_homs_hgnc_name(?Hgnc, ?Name).

Map predicate from HGNC unique integer identifier to unique gene name/description.

==
?- hgnc_homs_hgnc_name(19295, Name).
Name = 'lemur tyrosine kinase 3'.
==

*/
hgnc_homs_hgnc_name( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_name(X,Y) ).

/** hgnc_homs_symb_hgnc(?Symb, ?Hgnc).

Map predicate from HGNC unique symbol to unique HGNC integer identifier.

==
?- hgnc_homs_symb_hgnc('LMTK3', HGNC).
HGNC = 19295.
==
*/
hgnc_homs_symb_hgnc( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_symb_hgnc(X,Y) ).

/** hgnc_homs_syno_symb(?Syno, ?Symb).

Map predicate from gene synonyms to approved HGNC Symbol.

==
?- hgnc_homs_syno_symb('LMR3', Symb).
Symb = 'LMTK3'.
==
*/
hgnc_homs_syno_symb( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_syno_symb(X,Y) ).

/** hgnc_homs_prev_symb( ?Prev, ?Symb).

Map predicate from previously known-as gene names to approved HGNC Symbol.

==
?- hgnc_homs_prev_symb('ERBB', Symb).
Symb = 'EGFR'.
==
*/
hgnc_homs_prev_symb( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_prev_symb(X,Y) ).

/** hgnc_homs_ccds_hgnc(?Ccds, ?Hgnc).

Map predicate from concesus protein coding regions to HGNC ID.

==
?- hgnc_homs_ccds_hgnc('CCDS11576', Hgnc).
Hgnc = 11979.
==
*/
hgnc_homs_ccds_hgnc( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_ccds_hgnc(X,Y) ).

/** hgnc_homs_hgnc_ccds(?Hgnc, ?Ccds).

Map predicate from HGNC ID to concesus protein coding regions.

==
?- hgnc_homs_hgnc_ccds(11979,  Ccds).
Ccds = 'CCDS11576'.

==
*/
hgnc_homs_hgnc_ccds( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_ccds(X,Y) ).

/** hgnc_homs_ensg_hgnc(?Ensg, ?Symb).

Map predicate from Ensembl gene id to HGNC Id.

==
?- hgnc_homs_ensg_hgnc(Ensg, 19295).
Ensg = 'ENSG00000142235'.
==
*/
hgnc_homs_ensg_hgnc( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_ensg_hgnc(X,Y) ).

/** hgnc_homs_symb_ncbi(?Symb, ?Ncbi).

Map predicate from HGNC symbols to (NCBI) entrez gene ids.

==
?- hgnc_homs_symb_ncbi('LMTK3', Entz).
Entz = 114783.
==
*/
hgnc_homs_symb_ncbi( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_symb_ncbi(X,Y) ).

/** hgnc_homs_ncbi_hgnc(?Ncbi, ?Symb).

Map predicate from entrez ids to approved HGNC Symbol.

==
?- hgnc_homs_ncbi_hgnc(114783, Symb).
Symb = 'LMTK3'.
==
*/
hgnc_homs_ncbi_hgnc( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_ncbi_hgnc(X,Y) ).

/** hgnc_homs_ncbi_symb(?Ncbi, ?Symb).

Map predicate from entrez ids to approved HGNC Symbol.

==
?- hgnc_homs_ncbi_symb(114783, Symb).
Symb = 'LMTK3'.
==
*/
hgnc_homs_ncbi_symb( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_ncbi_symb(X,Y) ).

/** hgnc_homs_hgnc_chrb(+Hgnc, -ChrB).

Map predicate from HGNC ID to Chromosome Band 

==
?- hgnc_homs_hgnc_chrb(5, ChrB).
ChrB = '19q13.43'
==
*/
hgnc_homs_hgnc_chrb( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_chrb(X,Y) ).


/** hgnc_homs_hgnc_ensg(+Hgnc, -EnsG).

Map predicate from HGNC ID to Ensembl Gene

==
?- hgnc_homs_hgnc_ensg(Hgnc, EnsG).
==

*/
hgnc_homs_hgnc_ensg( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_ensg(X,Y) ).

/** hgnc_homs_hgnc_ncbi(+Hgnc, -Ncbi).

Map predicate from HGNC ID to Ensembl Gene (by all means available)

==
?- hgnc_homs_hgnc_ncbi(Hgnc, Ncbi).
==

*/
hgnc_homs_hgnc_ncbi( X, Y ) :-
    bio_db:bio_db_serve( hgnc_homs_hgnc_ncbi(X,Y) ).


