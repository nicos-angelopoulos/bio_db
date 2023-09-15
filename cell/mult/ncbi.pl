:- module( bio_db_mult_ncbi, [
                bio_db_mult_ncbi/0,
                ncbi_mult_taxo_scnm/2,
                ncbi_mult_taxo_gbnm/2
             ]
         ).


/**  bio_db_suss_ncbi.

Documentation predicate for multi organism data from NCBI databases.

Defined predicates:
  * ncbi_mult_taxo_scnm/2
  * ncbi_mult_taxo_gbnm/2

@author nicos angelopoulos
@version  0.1 2023/9/15
@see bio_db_mult/0

*/
bio_db_mult_ncbi.

/**  ncbi_mult_taxo_scnm( ?Taxo, ?ScName ).

Taxonomy id (integer) to scientific name.

==
?- ncbi_mult_taxo_scnm(T, S).
==
*/
ncbi_mult_taxo_scnm( Taxo, Scnm ) :-
    bio_db:bio_db_serve( ncbi_mult_taxo_scnm(Taxo,Scnm) ).

/**  ncbi_mult_taxo_gbnm( ?Taxo, ?GenbankName ).

Taxonomy id (integer) to Genbank name.

==
?- ncbi_mult_taxo_gbnm(T, G).
==

*/
ncbi_mult_taxo_gbnm( Taxo, Gbnm ) :-
    bio_db:bio_db_serve( ncbi_suss_ensp_ncbi(Taxo,Gbnm) ).
