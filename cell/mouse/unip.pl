:- module( bio_db_mouse_unip, [
                bio_db_mouse_unip/0,
                %       (mouse) uniprot (Swiss Prot + Trembl)
                map_unip_mouse_ensp_unip/2,
                map_unip_mouse_mgim_unip/2,
                map_unip_mouse_trem_nucs/2,
                map_unip_mouse_unip_entz/2,
                map_unip_mouse_unip_symb/2,
                map_unip_mouse_unip_unig/2,
                map_unip_mouse_gyno_unip/2
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_mouse_unip.

Documentation predicate for Homo sapiens data from Uniprot database.

==
?- lib( &bio_db(mouse(unip)) ).
?- [ pack('bio_db/cell/mouse/unip') ].
==

@author nicos angelopoulos
@version  0.1 2018/11/3

*/
bio_db_mouse_unip.

/**  map_unip_mouse_ensp_unip( +Ensp, -UniP ).

Map predicate from Ensembl protein ids to Uniprot protein ids.

== 
?- map_unip_mouse_ensp_unip( Ensp, Unip ).
==

*/
map_unip_mouse_ensp_unip( X, Y ) :-
    bio_db:bio_db_serve( map_unip_mouse_ensp_unip(X,Y) ).

/**  map_unip_mouse_mgim_unip( ?UniP, ?Hgnc ).

Map predicate from Uniprot proteins to HGNC ids.

==
?-  map_unip_mouse_mgim_unip( 'Q96Q04', Hgnc ).
==
*/
map_unip_mouse_mgim_unip( X, Y ) :-
    bio_db:bio_db_serve( map_unip_mouse_mgim_unip(X,Y) ).

/**  map_unip_mouse_trem_nucs( +Trem, -Nucs ).

Map predicate from Trembl ids to nucleotide sequences.

==
?- map_unip_mouse_trem_nucs( Trem, Nucs ).
==
*/
map_unip_mouse_trem_nucs( X, Y ) :-
    bio_db:bio_db_serve( map_unip_mouse_trem_nucs(X,Y) ).

/**  map_unip_mouse_unip_entz( ?Unip, ?Entz ).

Map predicate from Uniprot Entrez ids.

==
?- map_unip_mouse_unip_entz( Unip, Entz ).
==

*/
map_unip_mouse_unip_entz( Unip, Entz ) :-
    bio_db:bio_db_serve( map_unip_mouse_unip_entz(Unip,Entz) ).
    
/**  map_unip_mouse_unip_symb( ?Unip, ?Symb ).

Map predicate from Uniprot to (Mgim) Symbol, as recorded at Uniprot database.<br>
Note that there is a mgim predicate that also maps these, but it produces non-identical results.

==
?- map_unip_mouse_unip_symb( UniP, Symb ).
==
*/
map_unip_mouse_unip_symb( Trem, Seqn ) :-
    bio_db:bio_db_serve( map_unip_mouse_unip_symb(Trem,Seqn) ).

/*  map_unip_mouse_unip_unig( ?UniP, ?UniG).

Map predicate from Uniprot protein to Uniprot Gene.

==
?- map_unip_mouse_unip_unig( UniP, UniG ).
==
*/
map_unip_mouse_unip_unig( X, Y ) :-
    bio_db:bio_db_serve( map_unip_mouse_unip_unig(X,Y) ).

/*  map_unip_mouse_gyno_unip( ?Symbol, ?UniP ).

Map predicate from gene symbol synonym to uniprot accession.
Not entirely sure, which symbol annotation is used, but it is useful 
for hunting withdrawn proteins (via symbol mapping). 

As far as I can see Uniprot does not provide a database of withdrawn protein ids.
So this can be used as a substitute.

==
?- map_unip_mouse_gyno_unip( 'Tmem254', UniP ).
UniP = 'P0DN89'.
==

@author Nicos Angelopoulos
@version  0.1 2018/12/03

*/

map_unip_mouse_gyno_unip( X, Y ) :-
    bio_db:bio_db_serve( map_unip_mouse_gyno_unip(X,Y) ).
