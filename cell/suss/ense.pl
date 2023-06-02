:- module( bio_db_suss_ense, [
               bio_db_suss_ense/0,
               ense_suss_ensg_chrl/5,
               ense_suss_ensg_symb/2,
               ense_suss_enst_chrl/5,
               ense_suss_enst_ensg/2
             ]
         ).


/**  bio_db_suss_ense.

Documentation predicate for pig (sus scrofa) data from Ensembl databases.

Ensembl generated predicates:
 * ense_suss_ensg_chrl/5
 * ense_suss_ensg_symb/2
 * ense_suss_enst_chrl/5
 * ense_suss_enst_ensg/2

@author nicos angelopoulos
@version  0.1 2023/6/3

*/
bio_db_suss_ense.

/**  ense_suss_ensg_symb( ?EnsG, ?MgimID ).

Ensembl gene id to symbol with data drawn from Ensembl.

==
?- ense_suss_ensg_symb( A, B ).
==

*/
ense_suss_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( ense_suss_ensg_symb(EnsG,Symb) ).

/**  ense_suss_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_suss_enst_chrl( EnsT, Chr, Start, End, Dir ).
==

*/
ense_suss_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_suss_enst_chrl(EnsT,Chr,Start,End,Dir) ).


/*  ense_suss_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene id to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_suss_ensg_chrl( EnsG, Chr, Start, End, Dir ).
==
*/
ense_suss_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_suss_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  ense_suss_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene id with data drawn from Ensembl.

==
?- ense_suss_enst_ensg( EnsT, EnsG ).
==
*/
ense_suss_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( ense_suss_enst_ensg(EnsT,EnsG) ).
