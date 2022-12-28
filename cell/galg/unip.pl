:- module( bio_db_galg_unip, [
                bio_db_galg_unip/0,
                    %       + Uniprot id to:
                unip_galg_unip_ensp/2,   % -> ensemblprotein id
                unip_galg_unip_ncbi/2,   % -> entrez gene id
                unip_galg_unip_gyno/2,   % -> synonym
                unip_galg_unip_strp/2,   % -> STRING protein id
                unip_galg_unip_symb/2    % -> symbol
                ] ).



:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_galg_unip.

Documentation predicate for chicken (gallus gallus) data from Uniprot database.

==
?- lib( & bio_db(galg(unip)) ).
?- [ pack('bio_db/cell/galg/unip') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/21

*/
bio_db_galg_unip.

/**  unip_galg_unip_symb( ?Unip, ?Symb ).

Map predicate from Uniprot unique integer identifier to unique gene symbol.

==
?- 
     unip_galg_unip_symb( Unip, Symb ).

==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
unip_galg_unip_symb( X, Y ) :-
    bio_db:bio_db_serve( unip_galg_unip_symb(X,Y) ).

/**  unip_galg_unip_strp( ?Unip, ?StringProtein).

Map predicate from Uniprot unique integer identifier to String protein.

==
?- unip_galg_unip_strp( Unip, StrP ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
unip_galg_unip_strp( X, Y ) :-
    bio_db:bio_db_serve( unip_galg_unip_strp(X,Y) ).

/**  unip_galg_unip_gyno( ?Unip, ?Syno ).

Map predicate from Uniprot ids to gene synonyms.

==
?- unip_galg_unip_gyno( Unip, '' ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21
*/
unip_galg_unip_gyno( X, Y ) :-
    bio_db:bio_db_serve( unip_galg_unip_gyno(X,Y) ).

/** unip_galg_unip_ncbi( ?Unip, ?Ncbi ).

Map predicate from Uniprot ids to entrez gene ids.

==
?- unip_galg_unip_ncbi( Unip, Ncbi ).

==
*/
unip_galg_unip_ncbi( X, Y ) :-
    bio_db:bio_db_serve( unip_galg_unip_ncbi(X,Y) ).

/** unip_galg_unip_ensp( ?Unip, ?Ensp ).

Map predicate from Uniprot id to Ensebml protein ID.

==
?- 
     unip_galg_unip_ensp( Unip, Ensp ).
==

@author nicos angelopoulos
@version  0:1 2022/12/21

*/
unip_galg_unip_ensp( X, Y ) :-
    bio_db:bio_db_serve( unip_galg_unip_ensp(X,Y) ).
