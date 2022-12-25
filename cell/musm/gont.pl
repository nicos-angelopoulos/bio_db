:- module( bio_db_musm_gont, [
                bio_db_musm_gont/0,
                %       + Gene ontology
                gont_musm_mgim_gont/4,
                gont_musm_gont_symb/4
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_musm_gont.

Documentation predicate for mouse data from gene ontology database.

==
?- lib( &bio_db(musm(gont)) ).
?- [pack('bio_db/cell/musm/gont')].
==

@author nicos angelopoulos
@version  0.1 2018/11/13
@version  0.2 2022/12/25

*/
bio_db_musm_gont.

/** gont_musm_mgim_gont( +Mgim, -Relation, -Evidence, -GoTerm ).

MGI marker to gene ontology term and evidence.

== 
?- map_mgim_musm_mgim_symb(Mgim,'Lmtk3'), 
   gont_musm_mgim_gont(Mgim, Rel, Evi, GO ), 
   write( Mgim-Rel-Evi-GO ), nl, fail.

?- map_mgim_musm_mgim_symb(Mgim,'Lmtk3'),
   gont_musm_mgim_gont(Mgim, Rel, Evi, GO ),
   write( Mgim-Rel-Evi-GO ), nl, fail.
3039582-acts_upstream_of_or_within-IEA-16310
3039582-enables-IBA-4672
3039582-enables-IEA-166
3039582-enables-IEA-4674
3039582-enables-IEA-5524
3039582-enables-IEA-16301
3039582-enables-IEA-16740
3039582-enables-IEA-46872
3039582-involved_in-IBA-6468
3039582-located_in-IEA-5794
3039582-located_in-IEA-16020
3039582-located_in-IEA-42995
false.
==

@version 0:1 2019/4/6
@version 0:2 2019/5/8, added evidence
@version 0:3 2022/12/17, added Relation
*/
gont_musm_mgim_gont( Mgi, Rel, Evid, Gont ) :-
    bio_db:bio_db_serve( gont_musm_mgim_gont(Mgi,Rel,Evid,Gont) ).

/** gont_musm_gont_symb( +GoTerm, -Relation, -Evidence, -Symb ).

GO term to mouse Symbol.

== 
?- gont_musm_gont_symb( GO, Rel, Evi, 'Lmtk3' ), write( GO-Rel-Evi ), nl, fail.
166-enables-IEA
4672-enables-IBA
4674-enables-IEA
5524-enables-IEA
5794-located_in-IEA
6468-involved_in-IBA
16020-located_in-IEA
16301-enables-IEA
16310-acts_upstream_of_or_within-IEA
16740-enables-IEA
42995-located_in-IEA
46872-enables-IEA
false.
==

@version 0:1 2019/4/6
@version 0:2 2019/5/8, added evidence
@version 0:3 2022/12/17, added Relation

*/
gont_musm_gont_symb( Gont, Rel, Evid, Symb ) :-
    bio_db:bio_db_serve( gont_musm_gont_symb(Gont,Rel,Evid,Symb) ).
