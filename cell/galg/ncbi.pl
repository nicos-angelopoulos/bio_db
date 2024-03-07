:- module( bio_db_galg_ncbi, [
                bio_db_galg_ncbi/0,
                ncbi_galg_ensg_ncbi/2,
                ncbi_galg_ensp_ncbi/2,
                ncbi_galg_ncbi_ensg/2,
                ncbi_galg_ncbi_ensp/2
             ]
         ).


/**  bio_db_galg_ncbi.

Documentation predicate for pig (sus scrofa) data from NCBI databases.

Defined predicates:
  * ncbi_galg_ensg_ncbi/2
  * ncbi_galg_ensp_ncbi/2
  * ncbi_galg_ncbi_ensg/2
  * ncbi_galg_ncbi_ensp/2

@author nicos angelopoulos
@version  0.1 2024/3/7
@see bio_db_galg/0

*/
bio_db_galg_ncbi.

/**  ncbi_galg_ensg_ncbi( ?EnsG, ?Ncbi ).

Ensembl gene id (atom) to NCBI number.

==
?- ncbi_galg_ensg_ncbi( A, B ).
==
*/
ncbi_galg_ensg_ncbi( EnsG, Symb ) :-
    bio_db:bio_db_serve( ncbi_galg_ensg_ncbi(EnsG,Symb) ).

/**  ncbi_galg_ensp_ncbi(?EnsP, ?Ncbi).

Ensembl protein (atom) to Ncbi number.

==
?- ncbi_galg_ensp_ncbi(EnsP, Ncbi).
==

*/
ncbi_galg_ensp_ncbi( EnsP, Ncbi ) :-
    bio_db:bio_db_serve( ncbi_galg_ensp_ncbi(EnsP,Ncbi) ).


/*  ncbi_galg_ncbi_ensg(?Ncbi, ?EnsG).

Ncbi accession number to Ensembl gene id (atom).

==
?- ncbi_galg_ncbi_ensg(Ncbi, EnsG).
==
*/
ncbi_galg_ncbi_ensg( Ncbi, EnsG ) :-
    bio_db:bio_db_serve( ncbi_galg_ncbi_ensg(Ncbi,EnsG) ).

/**  ncbi_galg_ncbi_ensp(+Ncbi, -EnsG).

Ncbi accession number to Ensembl proteing id (atom).

==
?- ncbi_galg_ncbi_ensp(Ncbi, EnsP).
==
*/
ncbi_galg_ncbi_ensp( Ncbi, EnsP ) :-
    bio_db:bio_db_serve( ncbi_galg_ncbi_ensp(Ncbi,EnsP) ).
