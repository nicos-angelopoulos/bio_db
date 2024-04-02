:- module( bio_db_suss_vgnc, [
                bio_db_suss_vgnc/0,
                vgnc_suss_vgnc_ensg/2,
                vgnc_suss_vgnc_name/2,
                vgnc_suss_vgnc_ncbi/2,
                vgnc_suss_vgnc_symb/2
             ]
         ).


/**  bio_db_suss_vgnc.

Documentation predicate for pig (sus scrofa) data from VGNC databases.

Defined db predicates:
  * vgnc_suss_vgnc_name/2
  * vgnc_suss_vgnc_symb/2
  * vgnc_suss_vgnc_ncbi/2

@author nicos angelopoulos
@version  0.1 2023/6/2
@version  0.2 2024/3/26, added vgnc_suss_vgnc_ncbi/2
@see bio_db_suss/0

*/
bio_db_suss_vgnc.

/**  vgnc_suss_vgnc_ensg( ?Vgnc, ?Ncbi ).

Vgnc to Ensembl gene (ENSG..).

*/
vgnc_suss_vgnc_ensg( EnsG, Ncbi ) :-
    bio_db:bio_db_serve( vgnc_suss_vgnc_ensg(EnsG,Ncbi) ).


/**  vgnc_suss_vgnc_name( ?Vgnc, ?Name ).

Vgnc to gene name.

==
?-  vgnc_suss_vgnc_symb(V, 'LMTK3' ),
    vgnc_suss_vgnc_name(V, N).

V = 89769,
N = 'lemur tyrosine kinase 3'.
==
*/
vgnc_suss_vgnc_name( EnsG, Symb ) :-
    bio_db:bio_db_serve( vgnc_suss_vgnc_name(EnsG,Symb) ).


/**  vgnc_suss_vgnc_ncbi( ?Vgnc, ?Ncbi ).

Vgnc to Ncbi.

*/
vgnc_suss_vgnc_ncbi( EnsG, Ncbi ) :-
    bio_db:bio_db_serve( vgnc_suss_vgnc_ncbi(EnsG,Ncbi) ).

/**  vgnc_suss_vgnc_symb(?Vgnc, ?Symb).

Vgnc to gene symbol.

==
?- vgnc_suss_vgnc_symb(V,'LMTK3').
V = 89769.
==

*/
vgnc_suss_vgnc_symb( Vgnc, Symb ) :-
    bio_db:bio_db_serve( vgnc_suss_vgnc_symb(Vgnc,Symb) ).

