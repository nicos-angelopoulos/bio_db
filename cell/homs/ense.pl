:- module( bio_db_hs_ense, [
                bio_db_hs_ense/0,
                %       + Ense-mbl
                map_ense_ensg_hgnc/2,
                map_ense_ensg_symb/2,
                map_ense_enst_chrl/5,
                map_ense_ensg_chrl/5,
                map_ense_enst_ensg/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_hs_ense.

Documentation predicate for Homo sapiens data from Ensembl databases.

==
?- lib( & bio_db(hs(ense)) ).
?- [ pack('bio_db/cell/hs/ense') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_ense.

/**  map_ense_ensg_hgnc( ?EnsG, ?Hgnc ).

Ensembl gene to HGNC ID with data drawn from Ensembl.

*/
map_ense_ensg_hgnc( EnsG, Hgnc ) :-
    bio_db:bio_db_serve( map_ense_ensg_hgnc(EnsG,Hgnc) ).

/**  map_ense_ensg_symb( ?EnsG, ?Hgnc ).

Ensembl gene to HGNC Symbol with data drawn from Ensembl.
*/
map_ense_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( map_ense_ensg_symb(EnsG,Symb) ).

/**  map_ense_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

*/
map_ense_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_enst_chrl(EnsT,Chr,Start,End,Dir) ).

/**  map_ense_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

*/
map_ense_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  map_ense_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene with data drawn from Ensembl.

*/
map_ense_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( map_ense_enst_ensg(EnsT,EnsG) ).
