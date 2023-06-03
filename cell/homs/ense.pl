:- module( bio_db_homs_ense, [
                bio_db_homs_ense/0,
                %       + Ense-mbl
                ense_homs_ensg_hgnc/2,
                ense_homs_ensg_symb/2,
                ense_homs_enst_chrl/5,
                ense_homs_ensg_chrl/5,
                ense_homs_enst_ensg/2
                ] ).

:- use_module(library(lib)).
:- lib(bio_db).

/**  bio_db_homs_ense.

Documentation predicate for Homo sapiens data from Ensembl databases.

Predicates defined:
  * ense_homs_ensg_hgnc/2
  * ense_homs_ensg_symb/2
  * ense_homs_enst_chrl/5
  * ense_homs_ensg_chrl/5
  * ense_homs_enst_ensg/2

==
?- lib( & bio_db(homs(ense)) ).
?- [ pack('bio_db/cell/homs/ense') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29
@version  0.2 2022/12/25

*/
bio_db_homs_ense.

/**  ense_homs_ensg_hgnc( ?EnsG, ?Hgnc ).

Ensembl gene to HGNC ID with data drawn from Ensembl.

*/
ense_homs_ensg_hgnc( EnsG, Hgnc ) :-
    bio_db:bio_db_serve( ense_homs_ensg_hgnc(EnsG,Hgnc) ).

/**  ense_homs_ensg_symb( ?EnsG, ?Hgnc ).

Ensembl gene to HGNC Symbol with data drawn from Ensembl.

*/
ense_homs_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( ense_homs_ensg_symb(EnsG,Symb) ).

/**  ense_homs_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

*/
ense_homs_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_homs_enst_chrl(EnsT,Chr,Start,End,Dir) ).

/**  ense_homs_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

*/
ense_homs_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_homs_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  ense_homs_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene with data drawn from Ensembl.

*/
ense_homs_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( ense_homs_enst_ensg(EnsT,EnsG) ).
