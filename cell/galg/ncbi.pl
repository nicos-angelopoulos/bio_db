:- module( bio_db_galg_ncbi, [
                bio_db_galg_ncbi/0,
                ncbi_galg_dnuc_symb/2,
                ncbi_galg_ncbi_ensg/2,
                ncbi_galg_ncbi_ensp/2,
                ncbi_galg_ncbi_symb/2,
                ncbi_galg_nsyn_symb/2,
                ncbi_galg_rnuc_symb/2 
             ]
         ).


/**  bio_db_galg_ncbi.

Documentation predicate for pig (sus scrofa) data from NCBI databases.

Defined predicates:
  * ncbi_galg_dnuc_symb/2
  * ncbi_galg_ncbi_ensg/2
  * ncbi_galg_ncbi_ensp/2
  * ncbi_galg_ncbi_symb/2
  * ncbi_galg_nsyn_symb/2
  * ncbi_galg_rnuc_symb/2 

@author nicos angelopoulos
@version  0.1 2024/3/7
@see bio_db_galg/0

*/
bio_db_galg_ncbi.

/** ncbi_galg_dnuc_symb(DnaNucl, Symb).

Map predicate from DNA nucleic sequence to NCBI symbol.

==
?- ncbi_galg_dnuc_symb(DnaNucl, Symb).
==
*/
ncbi_galg_dnuc_symb( Dnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_galg_dnuc_symb(Dnuc,Symb) ).

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

/** ncbi_galg_ncbi_symb( ?Ncbi, ?Symb).

Map predicate from NCBI/entrez gene ids to Symbols. 

Note that the Symbols are no checked against HGNC. They are what NCBI calls symbols.

==
?- ncbi_galg_ncbi_symb( Ncbi, Symb ).
==
*/
ncbi_galg_ncbi_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_galg_ncbi_symb(X,Y) ).

/** ncbi_galg_nsyn_symb( ?Nsyn, ?Symb ).

Map of NCBI synonym to symbols.

==
?- ncbi_galg_nsyn_symb(Nsyn,'LRP1').
Nsyn = 'A2MR' ;
Nsyn = 'LRP-1'.
==
*/
ncbi_galg_nsyn_symb( X, Y ) :-
    bio_db:bio_db_serve( ncbi_galg_nsyn_symb(X,Y) ).

/** ncbi_galg_rnuc_symb( RnaNucl, Symb ).

Map predicate from RNA nucleic sequence to HGNC symbol.

==
?- ncbi_galg_rnuc_symb( Rnuc, Symb ).
==
*/
ncbi_galg_rnuc_symb( Rnuc, Symb ) :-
    bio_db:bio_db_serve( ncbi_galg_rnuc_symb(Rnuc,Symb) ).
