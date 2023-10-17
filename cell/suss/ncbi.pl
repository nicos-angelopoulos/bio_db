:- module( bio_db_suss_ncbi, [
                bio_db_suss_ncbi/0,
                ncbi_suss_ensg_ncbi/2,
                ncbi_suss_ensp_ncbi/2,
                ncbi_suss_ncbi_ensg/2,
                ncbi_suss_ncbi_ensp/2
             ]
         ).


/**  bio_db_suss_ncbi.

Documentation predicate for pig (sus scrofa) data from NCBI databases.

Defined predicates:
  * ncbi_suss_ensg_ncbi/2
  * ncbi_suss_ensp_ncbi/2
  * ncbi_suss_ncbi_ensg/2
  * ncbi_suss_ncbi_ensp/2

@author nicos angelopoulos
@version  0.1 2023/6/2
@see bio_db_suss/0

*/
bio_db_suss_ncbi.

/**  ncbi_suss_ensg_ncbi( ?EnsG, ?Ncbi ).

Ensembl gene id (atom) to NCBI number.

==
?- ncbi_suss_ensg_ncbi( A, B ).
==
*/
ncbi_suss_ensg_ncbi( EnsG, Symb ) :-
    bio_db:bio_db_serve( ncbi_suss_ensg_ncbi(EnsG,Symb) ).

/**  ncbi_suss_ensp_ncbi(?EnsP, ?Ncbi).

Ensembl protein (atom) to Ncbi number.

==
?- ncbi_suss_ensp_ncbi(EnsP, Ncbi).
==

*/
ncbi_suss_ensp_ncbi( EnsP, Ncbi ) :-
    bio_db:bio_db_serve( ncbi_suss_ensp_ncbi(EnsP,Ncbi) ).


/*  ncbi_suss_ncbi_ensg(?Ncbi, ?EnsG).

Ncbi accession number to Ensembl gene id (atom).

==
?- ncbi_suss_ncbi_ensg(Ncbi, EnsG).
==
*/
ncbi_suss_ncbi_ensg( Ncbi, EnsG ) :-
    bio_db:bio_db_serve( ncbi_suss_ncbi_ensg(Ncbi,EnsG) ).

/**  ncbi_suss_ncbi_ensp(+Ncbi, -EnsG).

Ncbi accession number to Ensembl proteing id (atom).

==
?- ncbi_suss_ncbi_ensp(Ncbi, EnsP).
==
*/
ncbi_suss_ncbi_ensp( Ncbi, EnsP ) :-
    bio_db:bio_db_serve( ncbi_suss_ncbi_ensp(Ncbi,EnsP) ).
