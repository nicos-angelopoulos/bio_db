:- module( bio_db_mouse_ense, [
                bio_db_mouse_ense/0,
                map_ense_mouse_ensg_mgim/2, 
                map_ense_mouse_ensg_symb/2,
                map_ense_mouse_enst_chrl/5,
                map_ense_mouse_ensg_chrl/5,
                map_ense_mouse_enst_ensg/2
             ]
         ).


/**  bio_db_hs_ense.

Documentation predicate for mouse (Mus musculus) data from Ensembl databases.

@author nicos angelopoulos
@version  0.1 2020/9/11

*/
bio_db_mouse_ense.

/**  map_ense_mouse_ensg_mgim( ?EnsG, ?MgimID ).

Ensembl gene id to MGIM id with data drawn from Ensembl.

==
?- map_ense_mouse_ensg_mgim( 'ENSMUSG00000062044', Mgim ).
Mgim = 3039582.
==
*/
map_ense_mouse_ensg_mgim( EnsG, Mgim ) :-
    bio_db:bio_db_serve( map_ense_mouse_ensg_mgim(EnsG,Mgim) ).

/**  map_ense_mouse_ensg_symb( ?EnsG, ?Symb ).

Ensembl gene id to MGIM or other Symbol. 

The predicate is ensembl centric, but takes consideration of MGIM combatibility in place.
Symb is the main MGIM symbol if the ensembl symbol is the same or it was a synonym to the
MGIM symbol. The ensbembl specific symbol is used if none of these is the case.

There is a broad agreement of symbols, with 55220 common symbols and only 267 ensembl specific
symbols (11th Sept. 2020).

==
?- map_ense_mouse_ensg_symb( EnGn, 'Lmtk3' ).
EnGn = 'ENSMUSG00000062044'.
==

*/
map_ense_mouse_ensg_symb( EnsG, Symb ) :-
    bio_db:bio_db_serve( map_ense_mouse_ensg_symb(EnsG,Symb) ).

/**  map_ense_mouse_enst_chrl( +EnsT, -Chr, -Start, -End, -Dir ).

Ensembl transcript chromosomal location.

Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- map_ense_mouse_enst_chrl( 'ENSMUST00000209617', Chr, Start, End, Dir ).
Chr = 7,
Start = 45783738,
End = 45804144,
Dir =  (+).
==

*/
map_ense_mouse_enst_chrl( EnsT, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_mouse_enst_chrl(EnsT,Chr,Start,End,Dir) ).


/*  map_ense_mouse_ensg_chrl( +EnsG, -Chr, -Start, -End, -Dir ).

Ensembl gene id to chromosomal location.
Chr is the chromosome, Start the start position, End the end position and 
Dir is the direction of the transcript.

==
?- map_ense_mouse_ensg_chrl( 'ENSMUSG00000062044', Chr, Start, End, Dir ).
Chr = 7,
Start = 45783738,
End = 45804144,
Dir =  (+).
==
*/
map_ense_mouse_ensg_chrl( EnsG, Chr, Start, End, Dir ) :-
    bio_db:bio_db_serve( map_ense_mouse_ensg_chrl(EnsG,Chr,Start,End,Dir) ).

/**  map_ense_mouse_enst_ensg( +EnsT, -EnsG ).

Ensembl Transcript to Ensembl Gene id with data drawn from Ensembl.

==
?- map_ense_mouse_enst_ensg( EnsT, 'ENSMUSG00000062044' ).
EnsT = 'ENSMUST00000209617' ;
EnsT = 'ENSMUST00000209701' ;
EnsT = 'ENSMUST00000072580' ;
EnsT = 'ENSMUST00000233503' ;
EnsT = 'ENSMUST00000120005' ;
EnsT = 'ENSMUST00000211609' ;
EnsT = 'ENSMUST00000211309' ;
EnsT = 'ENSMUST00000209351' ;
EnsT = 'ENSMUST00000211127'.
==
*/
map_ense_mouse_enst_ensg( EnsT, EnsG ) :-
    bio_db:bio_db_serve( map_ense_mouse_enst_ensg(EnsT,EnsG) ).
