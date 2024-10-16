:- module( bio_db_galg_ense, [
                bio_db_galg_ense/0,
                ense_gg6a_enst_chrl/5,
                ense_gg6a_ensg_chrl/5,
                ense_gg6a_enst_ensg/2,
                ense_gg6a_ensg_symb/2,
                ense_galg_enst_chrl/5,
                ense_galg_ensg_chrl/5,
                ense_galg_enst_ensg/2,
                ense_galg_ensg_symb/2,
                ense_gg7w_enst_chrl/5,
                ense_gg7w_ensg_chrl/5,
                ense_gg7w_enst_ensg/2,
                ense_gg7w_ensg_symb/2
             ]
         ).


/**  bio_db_galg_ense.

Documentation predicate for chicken (gallus gallus) data from Ensembl databases.

Ensembl provides genome information on 3 breeds of chicken, 

  * g6a('red jungle fowl')
    predicates: ense_gg6a
  * g7b(broiler)
    predicates: ense_galg
  * g7w('white leghorn layer')
    predicates: ense_gg7w


Unless you are certain you want one of the _7s_, my understanding is that 
you should use gg6a predicates. _7b_ is the default by means of hogging the _galg_ token,
because is what _Ensembl_ provides at the unqualified _gallus_gallus_ directory.

@author nicos angelopoulos
@version  0.1 2022/12/19
@version  0.2 2024/10/16, added gg7w preds

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
