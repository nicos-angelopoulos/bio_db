:- module( bio_db_homs_gont, [
                bio_db_homs_gont/0,
                %       + Gene Ontology.
                % A) edges
                gont_homs_edge_gisa/2,
                gont_homs_edge_greg/2,                % A regulates B
                gont_homs_edge_gprg/2,                % positively regulates
                gont_homs_edge_gnrg/2,                % negatively regulates
                gont_homs_edge_gpof/2,                % part_of
                % B) maps
                gont_homs_gont_symb/3,
                gont_homs_gont_gonm/2,
                gont_homs_symb_gont/3
                ] ).

:- use_module(library(lib)).
:- lib(& bio_db).

/**  bio_db_homs_gont.

Documentation predicate for Homo sapiens data from GO database.

==
?- lib( & bio_db(homs(gont)) ).
?- [ pack('bio_db/cell/homs/gont') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_homs_gont.

%% gont_homs_edge_gisa( ?Ch, ?Pa ).
%
% Gene ontotology is_a relation. Ch (a GO term) is_a (part of) Pa (a GO term).
%
%==
%  ?- gont_homs_edge_gisa(G1,G2), gont_homs_gont_gonm( G1, N1 ), map_homs_gont_gont_gonm( G2, N2 ).
%  G1 = 'GO:0000001',
%  G2 = 'GO:0048308',
%  N1 = 'mitochondrion inheritance',
%  N2 = 'organelle inheritance' .
%==
%
gont_homs_edge_gisa( Ch, Pa ) :-
    bio_db:bio_db_serve( gont_homs_edge_gisa(Ch,Pa) ).

%% gont_homs_edge_greg( ?Pa, ?Ch ).
%
%  Pa regulates Ch (GO hirerchical relation).
%
gont_homs_edge_greg( Pa, Ch ) :-
    bio_db:bio_db_serve( gont_homs_edge_greg(Pa,Ch) ).

%% gont_homs_edge_gprg( ?Pa, ?Ch ).
%
%  Pa positively regulates Ch (GO hirerchical relation).
%
gont_homs_edge_gprg( Pa, Ch ) :-
    bio_db:bio_db_serve( gont_homs_edge_gprg(Pa,Ch) ).

%% gont_homs_edge_gnrg( ?Pa, ?Ch ).
%
%  Pa negatively regulates Ch (GO hirerchical relation).
%
gont_homs_edge_gnrg( Pa, Ch ) :-
    bio_db:bio_db_serve( gont_homs_edge_gnrg(Pa,Ch) ).

%% gont_homs_edge_gpof( ?Part, ?Whole ).
%
%  Part is part of Whole (GO hirerchical relation).
%
gont_homs_edge_gpof( Part, Whole ) :-
    bio_db:bio_db_serve( gont_homs_edge_gpof(Part,Whole) ).

/**  gont_homs_gont_symb( ?Gont, -Evid,  -Symb).

Map predicate from GO terms to approved HGNC Symbol.

==
?- map_gont_gont_symb( 'GO:0003674', _, Symb ).
Symb = 'A1BG' ;
Symb = 'AAAS' ;
Symb = 'AARSD1'...
==
*/
gont_homs_gont_symb( X, Ev, Y ) :-
    bio_db:bio_db_serve( gont_homs_gont_symb(X,Ev,Y) ).

/**  gont_homs_gont_gonm( ?Gont, ?Gonm ).

Map predicate from gene ontology terms to GO term names.

==
?- gont_homs_gont_gonm( 'GO:0004674', A ).
A = 'protein serine/threonine kinase activity'.
==
*/
gont_homs_gont_gonm( X, Y ) :-
    bio_db:bio_db_serve( gont_homs_gont_gonm(X,Y) ).

/**  gont_homs_symb_gont( +Symb, -Ev, -Gont ).

Map predicate from HGNC symbols to GO terms.

==
?- gont_homs_symb_gont( 'LMTK3', Symb ).
Symb = 'GO:0003674' ;
Symb = 'GO:0004674' ;
Symb = 'GO:0004713' ;
Symb = 'GO:0005524' ;
Symb = 'GO:0005575' ;
Symb = 'GO:0006468' ;
Symb = 'GO:0010923' ;
Symb = 'GO:0016021' ;
Symb = 'GO:0018108'.
==
*/
gont_homs_symb_gont( X, Ev, Y ) :-
    bio_db:bio_db_serve( gont_homs_symb_gont(X,Ev,Y) ).
