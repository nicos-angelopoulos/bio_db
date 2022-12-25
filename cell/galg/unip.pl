:- module( bio_db_gallus_unip, [
                bio_db_gallus_unip/0,
                    %       + Uniprot id to:
                map_unip_gallus_unip_ensp/2,   % -> ensemblprotein id
                map_unip_gallus_unip_entz/2,   % -> entrez gene id
                map_unip_gallus_unip_gyno/2,   % -> synonym
                map_unip_gallus_unip_strp/2,   % -> STRING protein id
                map_unip_gallus_unip_symb/2    % -> symbol
                ] ).



:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_gallus_unip.

Documentation predicate for chicken (gallus gallus) data from Uniprot database.

==
?- lib( & bio_db(gallus(unip)) ).
?- [ pack('bio_db/cell/gallus/unip') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/21

*/
bio_db_gallus_unip.

/**  map_unip_gallus_unip_symb( ?Unip, ?Symb ).

Map predicate from Uniprot unique integer identifier to unique gene symbol.

==
?- 
     map_unip_gallus_unip_symb( Unip, Symb ).

==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
map_unip_gallus_unip_symb( X, Y ) :-
    bio_db:bio_db_serve( map_unip_gallus_unip_symb(X,Y) ).

/**  map_unip_gallus_unip_strp( ?Unip, ?StringProtein).

Map predicate from Uniprot unique integer identifier to String protein.

==
?- map_unip_gallus_unip_strp( Unip, StrP ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
map_unip_gallus_unip_strp( X, Y ) :-
    bio_db:bio_db_serve( map_unip_gallus_unip_strp(X,Y) ).

/**  map_unip_gallus_unip_gyno( ?Unip, ?Syno ).

Map predicate from Uniprot ids to gene synonyms.

==
?- map_unip_gallus_unip_gyno( Unip, '' ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
map_unip_gallus_unip_gyno( X, Y ) :-
    bio_db:bio_db_serve( map_unip_gallus_unip_gyno(X,Y) ).

/** map_unip_gallus_unip_entz( ?Unip, ?Entz ).

Map predicate from Uniprot ids to entrez gene ids.

==
?- map_unip_gallus_unip_entz( Unip, Entz ).

==
*/
map_unip_gallus_unip_entz( X, Y ) :-
    bio_db:bio_db_serve( map_unip_gallus_unip_entz(X,Y) ).

/** map_unip_gallus_unip_ensp( ?Unip, ?Ensp ).

Map predicate from Uniprot id to Ensebml protein ID.

==
?- 
     map_unip_gallus_unip_ensp( Unip, Ensp ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21

*/
map_unip_gallus_unip_ensp( X, Y ) :-
    bio_db:bio_db_serve( map_unip_gallus_unip_ensp(X,Y) ).
