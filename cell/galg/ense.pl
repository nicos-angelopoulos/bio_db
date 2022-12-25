:- module( bio_db_galg_ense, [
                bio_db_galg_ense/0,
                ense_galg_enst_chrl/5,
                ense_galg_ensg_chrl/5,
                ense_galg_enst_ensg/2,
                ense_galg_ensg_symb/2,
                ense_gg6a_enst_chrl/5,
                ense_gg6a_ensg_chrl/5,
                ense_gg6a_enst_ensg/2,
                ense_gg6a_ensg_symb/2
             ]
         ).


/**  bio_db_galg_ense.

Documentation predicate for chicken (gallus gallus) data from Ensembl databases.

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_galg_ense.

/**  ense_galg_ensg_symb( ?EnsG, ?MgimID ).

Ensembl gene id to symbol with data drawn from Ensembl.

==
?- ense_galg_ensg_symb( A, B ).
==
*/
ense_galg_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( ense_galg_ensg_symb(EnsG,Symb) ).

/**  ense_galg_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_galg_enst_chrl( EnsT, Chr, Start, End, Dir ).
==

*/
ense_galg_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_galg_enst_chrl(EnsT,Chr,Start,End,Dir) ).


/*  ense_galg_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene id to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_galg_ensg_chrl( EnsG, Chr, Start, End, Dir ).
==
*/
ense_galg_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_galg_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  ense_galg_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene id with data drawn from Ensembl.

==
?- ense_mouse_enst_ensg( EnsT, EnsG ).
==
*/
ense_galg_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( ense_galg_enst_ensg(EnsT,EnsG) ).

/**  ense_gg6a_ensg_symb( ?EnsG, ?MgimID ).

Ensembl gene id to symbol with data drawn from Ensembl.

==
?- ense_gg6a_ensg_symb( A, B ).
==
*/
ense_gg6a_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( ense_gg6a_ensg_symb(EnsG,Symb) ).

/**  ense_gg6a_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_gg6a_enst_chrl( EnsT, Chr, Start, End, Dir ).
==

*/
ense_gg6a_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_gg6a_enst_chrl(EnsT,Chr,Start,End,Dir) ).


/*  ense_gg6a_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene id to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- ense_gg6a_ensg_chrl( EnsG, Chr, Start, End, Dir ).
==
*/
ense_gg6a_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( ense_gg6a_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  ense_gg6a_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene id with data drawn from Ensembl.

==
?- ense_mouse_enst_ensg( EnsT, EnsG ).
==
*/
ense_gg6a_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( ense_gg6a_enst_ensg(EnsT,EnsG) ).
