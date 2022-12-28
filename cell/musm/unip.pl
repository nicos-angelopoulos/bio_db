:- module( bio_db_musm_unip, [
                bio_db_musm_unip/0,
                %       (mouse) uniprot (Swiss Prot + Trembl)
                unip_musm_ensp_unip/2,
                unip_musm_mgim_unip/2,
                unip_musm_trem_nucs/2,
                unip_musm_unip_ncbi/2,
                unip_musm_unip_symb/2,
                % unip_musm_unip_unig/2,  % unigene was discontinued
                unip_musm_gyno_unip/2
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_musm_unip.

Documentation predicate for Homo sapiens data from Uniprot database.

==
?- lib( &bio_db(musm(unip)) ).
?- [ pack('bio_db/cell/musm/unip') ].
==

@author nicos angelopoulos
@version  0.1 2018/11/3

*/
bio_db_musm_unip.

/**  unip_musm_ensp_unip( +Ensp, -UniP ).

Map predicate from Ensembl protein ids to Uniprot protein ids.

== 
?- unip_musm_ensp_unip( Ensp, Unip ).
==

*/
unip_musm_ensp_unip( X, Y ) :-
    bio_db:bio_db_serve( unip_musm_ensp_unip(X,Y) ).

/**  unip_musm_mgim_unip( +Mgim, -Unip ).

Map predicate from Mgi markers to Uniprot proteins.

==
?- map_mgim_musm_mgim_symb( Mgim, 'Lmtk3' ), unip_musm_mgim_unip( Mgim, Lmtk3Prot ), write( Lmtk3Prot ), nl, fail.
A0A0R4J0W7
A0A1B0GR61
A0A1B0GRD8
A0A1B0GSP4
A0A1B0GSR5
A0A1B0GSU1
Q5XJV6
==
*/
unip_musm_mgim_unip( X, Y ) :-
    bio_db:bio_db_serve( unip_musm_mgim_unip(X,Y) ).

/**  unip_musm_trem_nucs( +Trem, -Nucs ).

Map predicate from Trembl ids to nucleotide sequences.

==
?- unip_musm_trem_nucs( Trem, Nucs ).
==
*/
unip_musm_trem_nucs( X, Y ) :-
    bio_db:bio_db_serve( unip_musm_trem_nucs(X,Y) ).

/**  unip_musm_unip_ncbi( ?Unip, ?Ncbi ).

Map predicate from Uniprot NCBI/Entrez ids.

==
?- unip_musm_unip_ncbi( Unip, ncbi ).
==

*/
unip_musm_unip_ncbi( Unip, Ncbi ) :-
    bio_db:bio_db_serve( unip_musm_unip_ncbi(Unip,Ncbi) ).
    
/**  unip_musm_unip_symb( ?Unip, ?Symb ).

Map predicate from Uniprot to (Mgim) Symbol, as recorded at Uniprot database.<br>
Note that there is a mgim predicate that also maps these, but it produces non-identical results.

==
?- unip_musm_unip_symb( UniP, Symb ).
==
*/
unip_musm_unip_symb( Trem, Seqn ) :-
    bio_db:bio_db_serve( unip_musm_unip_symb(Trem,Seqn) ).

/*  unip_musm_unip_unig( ?UniP, ?UniG).

Map predicate from Uniprot protein to Uniprot Gene.

==
?- unip_musm_unip_unig( UniP, UniG ).
==
*/
unip_musm_unip_unig( X, Y ) :-
    bio_db:bio_db_serve( unip_musm_unip_unig(X,Y) ).

/*  unip_musm_gyno_unip( ?Symbol, ?UniP ).

Map predicate from gene symbol synonym to uniprot accession.
Not entirely sure, which symbol annotation is used, but it is useful 
for hunting withdrawn proteins (via symbol mapping). 

As far as I can see Uniprot does not provide a database of withdrawn protein ids.
So this can be used as a substitute.

==
?- unip_musm_gyno_unip( 'Tmem254', UniP ).
UniP = 'P0DN89'.
==

@author Nicos Angelopoulos
@version  0.1 2018/12/03

*/

unip_musm_gyno_unip( X, Y ) :-
    bio_db:bio_db_serve( unip_musm_gyno_unip(X,Y) ).
