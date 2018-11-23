:- module( bio_db_mouse_gont, [
                bio_db_mouse_gont/0,
                %       + Gene ontology
                map_gont_mouse_mgim_gont/3
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_mouse_gont.

Documentation predicate for mouse data from gene ontology database.

==
?- lib( &bio_db(mouse(gont)) ).
?- [pack('bio_db/cell/mouse/gont')].
==

@author nicos angelopoulos
@version  0.1 2018/11/13

*/
bio_db_mouse_gont.

/** map_gont_mouse_mgim_gont( +Mgim, -Evidence, -GoTerm ).

MGI marker to gene ontology term and evidence.

== 
?- map_mgim_mouse_mgim_symb(Mgim,'Lmtk3'), 
   map_gont_mouse_mgim_gont( Mgim, EV, GoTerm ).
==

*/
map_gont_mouse_mgim_gont( Mgi, Evid, Gont ) :-
    bio_db:bio_db_serve( map_gont_mouse_mgim_gont(Mgi,Evid,Gont) ).
