:- module( bio_db_gallus_cgnc, [
                bio_db_gallus_cgnc/0,
                    %       + Uniprot id to:
                map_unip_gallus_unip_ensp/2,   % -> ensemblprotein id
                map_unip_gallus_unip_entz/2,   % -> entrez gene id
                map_unip_gallus_unip_gyno/2,   % -> symbol synonym
                map_unip_gallus_unip_strp/2,   % -> STRING protein id
                map_unip_gallus_unip_symb/2    % -> symbol
                ] ).



:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_gallus_cgnc.

Documentation predicate for chicken (gallus gallus) data from CGNC database- Chicken Gene Nomenclature Committee).

==
?- lib( & bio_db(gallus(cgnc)) ).
?- [ pack('bio_db/cell/gallus/cgnc') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_gallus_cgnc.

/**  map_cgnc_gallus_cgnc_symb( ?Cgnc, ?Symb ).

Map predicate from CGNC unique integer identifier to unique gene symbol.

==
?- 
     map_cgnc_gallus_cgnc_symb( 20061, Symb ).

Symb = 'FKBP1A'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
map_cgnc_gallus_cgnc_symb( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_symb(X,Y) ).

/**  map_cgnc_gallus_cgnc_name( ?Cgnc, ?Name).

Map predicate from CGNC unique integer identifier to unique gene name/description.

==
?- map_cgnc_gallus_cgnc_name( 20061, Name ).
Name = 'FK506 binding protein 1A'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
map_cgnc_gallus_cgnc_name( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_name(X,Y) ).

/**  map_cgnc_gallus_cgnc_syno( ?CGNC, ?Syno ).

Map predicate from CGNC ids to gene synonyms- most of them are name like, a few are symbol like.

==
?- map_cgnc_gallus_cgnc_syno( CGNC, 'BMP-10' ).
CGNC = 60.

?- map_cgnc_gallus_cgnc_syno( 51, Syno ).
Syno = 'tripartite motif-containing protein 7'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
map_cgnc_gallus_cgnc_syno( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_syno(X,Y) ).

/** map_cgnc_gallus_cgnc_entz( ?CGNC, ?Entz ).

Map predicate from CGNC ids to entrez gene ids.

==
?- map_cgnc_gallus_cgnc_entz( 20061, Entz ).
Entz = 374233.

?- map_cgnc_gallus_cgnc_entz( 20064, Entz ).
Entz = 374240.

==
*/
map_cgnc_gallus_cgnc_entz( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_entz(X,Y) ).

/**  map_cgnc_gallus_cgnc_ensg( +Cgnc, -EnsG ).

Map predicate from CGNC id to Ensembl Gene id.

==
?- map_cgnc_gallus_cgnc_ensg(20064,EnsG).

EnsG = 'ENSGALG00010015997'.
==
*/
map_cgnc_gallus_cgnc_ensg( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_ensg(X,Y) ).

/** map_cgnc_gallus_cgnc_curs( ?CGNC, ?Curs ).

Map predicate from CGNC id to curation status.

==
?- 
     map_cgnc_gallus_cgnc_curs(20064,Curs).

Curs = 'Automatic'.

?-
     findall(Curs,map_cgnc_gallus_cgnc_curs(_,Curs),CursesEs),
     sort(CursesEs,Curses).

CursesEs = ['Approved', 'Approved', 'Automatic', 'Approved'|...],
Curses = ['Approved', 'Automatic'].

==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
map_cgnc_gallus_cgnc_curs( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_curs(X,Y) ).

/** map_cgnc_gallus_cgnc_edat( ?CGNC, ?Edat ).

Map predicate from CGNC id to last edit date.

==
?- 
     map_cgnc_gallus_cgnc_edat( 20064, Edat ).
Edat = '2022-05-05'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19

*/
map_cgnc_gallus_cgnc_edat( X, Y ) :-
    bio_db:bio_db_serve( map_cgnc_gallus_cgnc_edat(X,Y) ).
