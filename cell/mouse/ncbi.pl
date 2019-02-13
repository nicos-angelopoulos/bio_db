:- module( bio_db_mouse_ncbi, [
                bio_db_mouse_ncbi/0,
                %       (mouse) NCBI (entrez synonums only)
                map_ncbi_mouse_syno_symb/2
                ] ).

:- use_module( library(lib) ).
:- lib(bio_db).

/**  bio_db_mouse_ncbi.

Documentation predicate for mouse data from NCBI database.

==
?- lib( &bio_db(mouse(ncbi)) ).
?- [ pack('bio_db/cell/mouse/ncbi') ].

==

@author nicos angelopoulos
@version  0.1 2019/2/12

*/
bio_db_mouse_ncbi.

/**  map_ncbi_mouse_syno_symb( +Syno, -Symb ).

Map of symbol synonyms based on the NCBI (Entrez) data.

== 
?- map_ncbi_mouse_syno_symb( 'Znf638', Symb ).
Symb = 'Zfp638'.

?- map_ncbi_mouse_syno_symb( 'Stx5', Symb ).
Symb = 'Stx5a'.
==

*/
map_ncbi_mouse_syno_symb( X, Y ) :-
    bio_db:bio_db_serve( map_ncbi_mouse_syno_symb(X,Y) ).
