:- module( bio_db_hs_gont, [
                bio_db_hs_gont/0,
                %       + Gene Ontology.
                % A) edges
                % edge_gont_includes/2,                 % reciprocal of is_a
                % commenting out on version: 3:3:0
                edge_gont_is_a/2,
                edge_gont_regulates/2,                % A regulates B
                edge_gont_positively_regulates/2,
                edge_gont_negatively_regulates/2,
                % edge_gont_consists_of/2,              % reciprocal of part_of/2
                % commenting out on version: 3:3:0
                edge_gont_part_of/2,
                % B) maps
                map_gont_gont_symb/3,
                map_gont_gont_gonm/2,
                map_gont_symb_gont/3
                ] ).

:- use_module(library(lib)).
:- lib(& bio_db).

/**  bio_db_hs_gont.

Documentation predicate for Homo sapiens data from GO database.

==
?- lib( & bio_db(hs(gont)) ).
?- [ pack('bio_db/cell/hs/gont') ].
==

@author nicos angelopoulos
@version  0.1 2018/10/29

*/
bio_db_hs_gont.

%% edge_gont_includes( ?Pa, ?Ch ).
%
%  Reciprocal of edge_gont_is_a/2.
% 
edge_gont_includes( Pa, Ch ) :-
    bio_db:bio_db_serve( edge_gont_includes(Pa,Ch) ).

%% edge_gont_is_a( ?Ch, ?Pa ).
%
% Gene ontotology is_a relation. Ch (a GO term) is_a (part of) Pa (a GO term).
%
%==
%  ?- edge_gont_is_a(G1,G2), map_gont_gont_gonm( G1, N1 ), map_gont_gont_gonm( G2, N2 ).
%  G1 = 'GO:0000001',
%  G2 = 'GO:0048308',
%  N1 = 'mitochondrion inheritance',
%  N2 = 'organelle inheritance' .
%==
%
edge_gont_is_a( Ch, Pa ) :-
    bio_db:bio_db_serve( edge_gont_is_a(Ch,Pa) ).


%% edge_gont_regulates( ?Pa, ?Ch ).
%
%  Pa regulates Ch (GO hirerchical relation).
%
edge_gont_regulates( Pa, Ch ) :-
    bio_db:bio_db_serve( edge_gont_regulates(Pa,Ch) ).

%% edge_gont_positively_regulates( ?Pa, ?Ch ).
%
%  Pa positively regulates Ch (GO hirerchical relation).
%
edge_gont_positively_regulates( Pa, Ch ) :-
    bio_db:bio_db_serve( edge_gont_positively_regulates(Pa,Ch) ).

%% edge_gont_negatively_regulates( ?Pa, ?Ch ).
%
%  Pa negatively regulates Ch (GO hirerchical relation).
%
edge_gont_negatively_regulates( Pa, Ch ) :-
    bio_db:bio_db_serve( edge_gont_negatively_regulates(Pa,Ch) ).

%% edge_gont_part_of( ?Part, ?Whole ).
%
%  Part is part of Whole (GO hirerchical relation).
%
edge_gont_part_of( Part, Whole ) :-
    bio_db:bio_db_serve( edge_gont_part_of(Part,Whole) ).

/** edge_gont_consists_of( ?Whole, ?Part ).

Whole consists (in part) of Part (reciprocal of edge_gont_part_of/2).

==
?- edge_gont_part_of( A, B ),\+ edge_gont_consists_of( B, A).
false.
==

*/
edge_gont_consists_of( Whole, Part ) :-
    bio_db:bio_db_serve( edge_gont_consists_of(Whole,Part) ).

/**  map_gont_gont_symb( ?Gont, -Evid,  -Symb).

Map predicate from GO terms to approved HGNC Symbol.

==
?- map_gont_gont_symb( 'GO:0003674', Symb ).
Symb = 'A1BG' ;
Symb = 'AAAS' ;
Symb = 'AARSD1'...
==
*/
map_gont_gont_symb( X, Ev, Y ) :-
    bio_db:bio_db_serve( map_gont_gont_symb(X,Ev,Y) ).

/**  map_gont_gont_gonm( ?Gont, ?Gonm ).

Map predicate from gene ontology terms to GO term names.

==
?- map_gont_gont_gonm( 'GO:0004674', A ).
A = 'protein serine/threonine kinase activity'.
==
*/
map_gont_gont_gonm( X, Y ) :-
    bio_db:bio_db_serve( map_gont_gont_gonm(X,Y) ).

/**  map_gont_symb_gont( +Symb, -Ev, -Gont ).

Map predicate from HGNC symbols to GO terms.

==
?- map_gont_symb_gont( 'LMTK3', Symb ).
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
map_gont_symb_gont( X, Ev, Y ) :-
    bio_db:bio_db_serve( map_gont_symb_gont(X,Ev,Y) ).
