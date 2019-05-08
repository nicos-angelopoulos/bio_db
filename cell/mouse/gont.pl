:- module( bio_db_mouse_gont, [
                bio_db_mouse_gont/0,
                %       + Gene ontology
                map_gont_mouse_mgim_gont/3,
                map_gont_mouse_gont_symb/3
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

/** map_gont_mouse_gont_symb( +GoTerm, -Evidence, -Symb ).

GO term to mouse Symbol.

== 
?- map_gont_mouse_gont_symb(Mgim,'Lmtk3').
Go = 166 ;
Go = 4672 ;
Go = 4674 ;
Go = 5524 ;
Go = 5794 ;
Go = 6468 ;
Go = 10923 ;
Go = 16020 ;
Go = 16021 ;
Go = 16301 ;
Go = 16310 ;
Go = 16740 ;
Go = 42995 ;
Go = 46872.
==

@version 0:1 2019/4/6
@version 0:2 2019/5/8, added evidence

*/
map_gont_mouse_gont_symb( Gont, Evid, Symb ) :-
    bio_db:bio_db_serve( map_gont_mouse_gont_symb(Gont,Evid,Symb) ).
