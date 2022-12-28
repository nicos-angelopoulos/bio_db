:- module( bio_db_galg_cgnc, [
                bio_db_galg_cgnc/0,
                    %       + CGNC ids to:
                cgnc_galg_cgnc_curs/2,   % -> curation status
                cgnc_galg_cgnc_edat/2,   % -> last edit date
                cgnc_galg_cgnc_ncbi/2,   % -> entrez id
                cgnc_galg_cgnc_ensg/2,   % -> ensembl gene
                cgnc_galg_cgnc_name/2,   % -> name (text describing)
                cgnc_galg_cgnc_symb/2,   % -> symbol (short, compact name)
                cgnc_galg_cgnc_syno/2    % -> known synonym
                ] ).

:- use_module(library(lib)).
:- lib(&(bio_db)).

/**  bio_db_galg_cgnc.

Documentation predicate for chicken (gallus gallus) data from CGNC database- Chicken Gene Nomenclature Committee).

==
?- lib( & bio_db(galg(cgnc)) ).
?- [ pack('bio_db/cell/galg/cgnc') ].
==

@author nicos angelopoulos
@version  0.1 2022/12/19

*/
bio_db_galg_cgnc.

/**  cgnc_galg_cgnc_symb( ?Cgnc, ?Symb ).

Map predicate from CGNC unique integer identifier to unique gene symbol.

==
?- 
     cgnc_galg_cgnc_symb( 20061, Symb ).

Symb = 'FKBP1A'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
cgnc_galg_cgnc_symb( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_symb(X,Y) ).

/**  cgnc_galg_cgnc_name( ?Cgnc, ?Name).

Map predicate from CGNC unique integer identifier to unique gene name/description.

==
?- cgnc_galg_cgnc_name( 20061, Name ).
Name = 'FK506 binding protein 1A'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
cgnc_galg_cgnc_name( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_name(X,Y) ).

/**  cgnc_galg_cgnc_syno( ?CGNC, ?Syno ).

Map predicate from CGNC ids to gene synonyms- most of them are name like, a few are symbol like.

==
?- cgnc_galg_cgnc_syno( CGNC, 'BMP-10' ).
CGNC = 60.

?- cgnc_galg_cgnc_syno( 51, Syno ).
Syno = 'tripartite motif-containing protein 7'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
cgnc_galg_cgnc_syno( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_syno(X,Y) ).

/** cgnc_galg_cgnc_ncbi( ?CGNC, ?Ncbi ).

Map predicate from CGNC ids to entrez gene ids.

==
?- cgnc_galg_cgnc_ncbi( 20061, Ncbi ).
Ncbi = 374233.

?- cgnc_galg_cgnc_ncbi( 20064, Ncbi ).
Ncbi = 374240.

==
*/
cgnc_galg_cgnc_ncbi( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_ncbi(X,Y) ).

/**  cgnc_galg_cgnc_ensg( +Cgnc, -EnsG ).

Map predicate from CGNC id to Ensembl Gene id.

==
?- cgnc_galg_cgnc_ensg(20064,EnsG).

EnsG = 'ENSGALG00010015997'.
==
*/
cgnc_galg_cgnc_ensg( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_ensg(X,Y) ).

/** cgnc_galg_cgnc_curs( ?CGNC, ?Curs ).

Map predicate from CGNC id to curation status.

==
?- 
     cgnc_galg_cgnc_curs(20064,Curs).

Curs = 'Automatic'.

?-
     findall(Curs,cgnc_galg_cgnc_curs(_,Curs),CursesEs),
     sort(CursesEs,Curses).

CursesEs = ['Approved', 'Approved', 'Automatic', 'Approved'|...],
Curses = ['Approved', 'Automatic'].

==

@author nicos angelopoulos
@version  0:1 2022/12/19
*/
cgnc_galg_cgnc_curs( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_curs(X,Y) ).

/** cgnc_galg_cgnc_edat( ?CGNC, ?Edat ).

Map predicate from CGNC id to last edit date.

==
?- 
     cgnc_galg_cgnc_edat( 20064, Edat ).
Edat = '2022-05-05'.
==

@author nicos angelopoulos
@version  0:1 2022/12/19

*/
cgnc_galg_cgnc_edat( X, Y ) :-
    bio_db:bio_db_serve( cgnc_galg_cgnc_edat(X,Y) ).
