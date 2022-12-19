:- module( bio_db_gallus_ense, [
                bio_db_gallus_ense/0,
                map_ense_gallus_enst_chrl/5,
                map_ense_gallus_ensg_chrl/5,
                map_ense_gallus_enst_ensg/2,
                map_ense_gallus_ensg_symb/2
             ]
         ).


/**  bio_db_gallus_ense.

Documentation predicate for chicken (gallus gallus) data from Ensembl databases.

@author nicos angelopoulos
@version  0.1 2020/9/11

*/
bio_db_gallus_ense.

/**  map_ense_gallus_ensg_symb( ?EnsG, ?MgimID ).

Ensembl gene id to symbol with data drawn from Ensembl.

==
?- map_ense_gallus_ensg_symb( A, B ).
==
*/
map_ense_gallus_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( map_ense_gallus_ensg_Symb(EnsG,Symb) ).

/**  map_ense_gallus_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- map_ense_gallus_enst_chrl( EnsT, Chr, Start, End, Dir ).
==

*/
map_ense_gallus_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_gallus_enst_chrl(EnsT,Chr,Start,End,Dir) ).


/*  map_ense_gallus_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene id to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- map_ense_gallus_ensg_chrl( EnsG, Chr, Start, End, Dir ).
==
*/
map_ense_gallus_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_gallus_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  map_ense_gallus_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene id with data drawn from Ensembl.

==
?- map_ense_mouse_enst_ensg( EnsT, EnsG ).
==
*/
map_ense_gallus_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( map_ense_gallus_enst_ensg(EnsT,EnsG) ).
