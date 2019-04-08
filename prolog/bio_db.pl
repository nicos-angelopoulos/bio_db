%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Authors:       Nicos Angelopoulos
%    E-mail:        Nicos Angelopoulos http://stoics.org.uk/~nicos/sware/contact.html
%    Copyright (C): Nicos Angelopoulos, 2015-2019
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
   This program is free software; you can redistribute it and/or
    modify it under the terms of the MIT license

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*/
:- module( bio_db, [
                % This interface has now being split according to 
                % biological organisms, see files in cell/
                % 1. housekeeping:
                % bio_db/0,
                bio_db_close/1,
                bio_db_db_predicate/1,
                bio_db_data_predicate/4,
                bio_db_info/2,
                bio_db_info/3,
                bio_db_info/4,
                bio_db_interface/1,
                bio_db_interface/2,
                bio_db_install/2, bio_db_install/3,
                bio_db_paths/0,
                bio_db_source/2,
                bio_db_version/2,
                bio_db_citation/2,
                bio_db_close_connections/0,
                % 2 derived
                % A.symbols
                is_symbol/2,
                entz_symb/3,
                % B. gene ontology
                go_id/2,        % +/-Go, -/+Int
                go_id/3         % +GoOrInt, -Go, -Int
             ] ).

:- dynamic( bio_db_handle/5 ).

:- dynamic( '$bio_db_handle'/2 ). % this is needed for the asserted server preds 


:- ensure_loaded(library(lib)).

:- ensure_loaded('../src/bio_db_data_predicate').

:- lib(source(bio_db), homonyms(true)).
:- lib(stoics_lib:date_two_digit_dotted/1).
:- lib(go_id/2).
:- lib(is_symbol/2).
:- lib(entz_symb/3).
:- lib(end(bio_db)).

% :- initialization( lib(& bio_db, load_main(false)), after_load ).
:- initialization( lib(@(bio_db)), after_load ).

bio_db_organism(hs). % defaulty
bio_db_organism(mouse).

% this search path can be added to requires
% bio_db_map/2,
% map_ncbi_ensp_unip/2,
% map_ncbi_ensp_ensg/2,

/* was:
bio_db_interface_atom( prolog ).
bio_db_interface_atom( prosqlite ).
bio_db_interface_atom( berkeley ).
*/
bio_db_interface_atom( Iface ) :-
    bio_db_interface_extensions( Iface, _ ).

bio_db_interface_initialisation( null ). % so it exists, fixme: should nt this be prolog ?
bio_db_interface_initialisation( prosqlite ) :-
    use_module( library(prosqlite) ).
bio_db_interface_initialisation( berkeley ) :-
    use_module( library(bdb) ).
bio_db_interface_initialisation( rocks ) :-
    use_module( library(rocksdb) ).

bio_db_default_interface( prolog ).

:- Opts = [access(read_write),type(atom),keep(true)],
   bio_db_default_interface( Def ),
   create_prolog_flag( bio_db_interface, Def, Opts ).

:- Opts = [access(read_write),type(atom),keep(true)],
   create_prolog_flag( bio_db_pl_from_zip, user, Opts ).  % true/false/user

:- Opts = [access(read_write),type(atom),keep(true)],
   create_prolog_flag( bio_db_del_zip, user, Opts ).  % true/false/user, only asked for pl files

:- use_module( library(lib) ).
:- lib( source(bio_db), homonyms(true) ).

:- lib(options).
:- lib(pack_errors).

:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:url_file/2).
:- lib(stoics_lib:message_report/3).

:- lib(ui_yes_no/5).
:- lib(bio_db_map/2).
:- ensure_loaded( '../auxil/lib/bio_db_pl_info' ).   % /2.
:- lib( end(bio_db) ).

stoics( 'http://stoics.org.uk/~nicos/sware/packs/bio_db_repo/data' ).

/** <module> Access, use and manage big, biological datasets.

   Bio_db gives access to pre-packed biological databases and simplifies 
management and translation of biological data to Prolog friendly formats.

There are currently 2 major types of data supported: maps, and graphs.
Maps define product mappings, translations and memberships, while graphs define interactions which
can be visualised as weighed graphs (see bio_db_data_predicate/4 for a full list of 
statically generated list of bio_db data  predicates).

There are 2 prolog flags (see current_prolog_flag/2) that can control the behaviour of the 
library: bio_db_qcompile (def: true) and bio_db_interface (def: prolog).
When the first one is set to false, it can disable the compilation to 

Bio_db itself does include any of the datasets. You can either download the separate pack(bio_db_repo)
which contains all of the Prolog datasets.
bio_db_repo   will install all the Prolog database files. The single tar and gzipped file is  
246 Mb in size and the fully expanded version of a Prolog installation can take up to 3.1Gb. 
The precise size depends on how many tables 
are accessed at least once (each producing an expanded .pl and a .qlf file).
The current pack(bio_db_repo) holds a total 67 data predicates and serves 38710918 records.
This pack can be installed as per usual via
==
?- pack(bio_db_repo).
==

If you do not install all datasets, each data table will be auto-downloaded the first 
time you try to access some of its data.  Auto-downloading works 
transparently to the user, where a data set is downloaded by simply calling the predicate.


For example
==
?- map_hgnc_symb_hgnc( 'LMTK3', Hgnc ).
% prolog DB:table hgnc:map_hgnc_symb_hgnc/2 is not installed, do you want to download (Y/n) ? 
% Trying to get: url_file(http://www.stoics.org.uk/bio_db_repo/data/maps/hgnc/map_hgnc_symb_hgnc.pl,/usr/local/users/nicos/local/git/test_bio_db/data/maps/hgnc/map_hgnc_symb_hgnc.pl)
% Loading prolog db: /usr/local/users/nicos/local/git/test_bio_db/data/maps/hgnc/map_hgnc_symb_hgnc.pl
Hgnc = 19295.

?- bio_db_interface( prosqlite ).
% Setting bio_db_interface prolog_flag, to: prosqlite
true.

?- map_hgnc_prev_symb( Prv, Symb ).
% prosqlite DB:table hgnc:map_hgnc_prev_symb/2 is not installed, do you want to download (Y/n) ? 
% Trying to get: url_file(http://www.stoics.org.uk/bio_db_repo/data/maps/hgnc/map_hgnc_prev_symb.sqlite,/usr/local/users/nicos/local/git/test_bio_db/data/maps/hgnc/map_hgnc_prev_symb.sqlite)
false.

?- map_hgnc_prev_symb( Prv, Symb ).
% prosqlite DB:table hgnc:map_hgnc_prev_symb/2 is not installed, do you want to download (Y/n) ? 
% Trying to get: url_file(http://www.stoics.org.uk/bio_db_repo/data/maps/hgnc/map_hgnc_prev_symb.sqlite,/usr/local/users/nicos/local/git/test_bio_db/data/maps/hgnc/map_hgnc_prev_symb.sqlite)
% Loading prosqlite db: /usr/local/users/nicos/local/git/test_bio_db/data/maps/hgnc/map_hgnc_prev_symb.sqlite
Prv = 'A1BG-AS',
Symb = 'A1BG-AS1' .
==

See bio_db_data_predicate/4.

As of version 2.0 bio_db is formed of a number of hierarchically organised cells that 
can be loaded independently. This is because there now too many predicates and is also a devise
for better supporting organism specific data. There are currently two main cells, hs (human) and mouse. Each
sub-celled by data source of origin.

==
?- use_module(library(bio_db)).
==
Loads the whole interface (all cells), without the user needing to be aware of anything.
The only difference is that the user will not be able to see all the module predicates
at the first line of file pack(bio_db/prolog/bio_db.pl)).

==
?- lib(bio_db).
==
Also loads everything.

==
?- lib(& bio_db).
==
Loads the skeleton of the module (cells usually laod the module dependencies like this).

==
?- lib(& bio_db(hs)).
==
Loads _hs_ cell (and skeleton). _hs_ comprises of a number of sub-cells.

==
?- lib(& bio_db(hs(hgnc))).
==
Loads the hs/hgnc primary cell (and the skeleton).

In both the above loads, the following becomes available, however, the former load
also loads additional predicates for human, but non hgnc based.

==
?- map_hgnc_hgnc_symb( Hgnc, 'LMTK3' ).
Hgnc = 19295.
==

The following
==
?- use_module( pack('bio_db/cell/hs/hgnc') ).
==
also loads just the HGNC part of the human section of bio_db, but it is not a 
recommended way to do so.


Databases
  * Ensembl=ense
    Homo sapiens genes and proteins. Genes and trascripts mappings along with mapping to genomic location (latter not included in release yet)

  * HGNC=hgnc
    Hugo Gene Nomenclature Committee, http://www.genenames.org/

  * NCBI=ncbi
    NCBI

  * Uniprot=unip
    Protein database.

  * String 
    Protein-Protein interactions data base

  * MGI
    mouse specific datasets 

For each database a token with the same token means 
that the field is the unique identifier of the object
in that database.

Tokens
  * mouse
    mouse 
  * hs
    human, often missing as originally was the only organism supported)

  * symb
    HGNC gene symbol (short, unique name for genes)

  * name
    HGNC gene name (long, less standarised version of gene name)

  * prev
    HGNC previous gene symbol

  * syno
    HGNC gene symbol synonym

  * ensg 
    ensembl gene

  * enst
    ensembl transcript

  * ensp
    ensembl protein

  * gonm
    GO name of a term

  * pros
    Prosite protein family information

  * rnuc
    RNA nucleic sequence ID to HGNC symbol.

  * unig
    uniprotein gene id
    
  * sprt
    Swiss-Prot part of Uniprot (high quality, curated)

  * trem
    TrEMBL part of Uniprot (non curated)

  * mgim
    MGI Marker (identifier for Mouse Genome Informatics Markers)

The name convension for maps is
  ==
   ?- map_hgnc_hgnc_symb( Hgnc, Symb ).
   Hgnc = 1,
   Symb = 'A12M1~withdrawn' ;
   Hgnc = 2,
   Symb = 'A12M2~withdrawn' .

   ?- map_hgnc_hgnc_symb( 19295, Symb ).
   Symb = 'LMTK3'.

   ?- map_hgnc_symb_hgnc( 'LMTK3', Hgnc ).
  Hgnc = 19295.

  == 

  Where the first hgnc corresponds to the source database, the second identifies the first argument of the 
  map to be the unique identifier field for that database (here a positive integer starting at 1 and with no gaps),
  The last part of the predicate name corresponds to the second argument, which here is the unique Symbol 
  assigned to a gene by HGNC. In the current version of bio_db, all tokens in map filenames are 4 characters long.
  Map data for predicate Pname from database DB are looked for in DB(Pname.Ext) (see bio_db_paths/0).
  Extension, Ext, depends on the current bio_db database interface (see bio_db_interface/1), and it is sqlite if
  the interface is prosqlite and pl otherwise.

The name convesion for graphs is 
  == 
  ?- edge_string_hs_symb( Symb1, Symb2, W ).
  S1 = 'A1BG',
  S2 = 'ABAT',
  W = 360 ;
  S1 = 'A1BG',
  S2 = 'ABCC6',
  W = 158 .
  ==

Where only the first and second tokens, edge and string respectively, are controlled. The second token
indicates the database of origin. Graph data for predicate Pname from database DB are looked for in
bio_db_data(graphs/DB/Pname.Ext) (see bio_db_paths/1).
  Extension, Ext, depends on the current bio_db database interface (see bio_db_interface/1), and it is sqlite if
  the interface is prosqlite and pl otherwise.

Bio_db supports four db interfaces: prolog, prosqlite, berkeley and rocks.  The first one is via Prolog fact bases, which is the default. 
The second is an interface to SQLite via pack(prosqlite) while the third and fourth work with the SWI-Prolog packs bdb and rocksdb.
The underlying mechanisms are entirely transparent to the user. In order to use the sqlite data sources
pack(prosqlite) needs to be installed via the pack manager
==
 ?- pack_install( prosqlite ).
==

The user can control which interface is in use with the bio_db_interface/1 predicate.
==
 ?- bio_db_interface( Curr ).
 Curr = prolog.

 ?- bio_db_interface( prosqlite ).

 ?- bio_db_interface( Curr ).
 Curr = prosqlite.
==

The type of the interface of a bio_db data predicate is determined by the interface at the 
time of first call.

Once the user has initiated the serving of a predicate via calling a goal to it, it is then 
possible to have access to information about the dataset such as download date and sourle url. 

==
?- map_hgnc_hgnc_symb( Hgnc, Symb ).
Hgnc = 1,
Symb = 'A12M1~withdrawn' .

?- bio_db_info( map_hgnc_hgnc_symb/2, Key, Value ), write( Key-Value ), nl, fail.
interface-prolog
source_url-ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz
datetime-datetime(2018,11,27,12,32,11)
data_types-data_types(integer,atom)
unique_lengths-unique_lengths(46023,46023,46023)
relation_type-relation_type(1,1)
header-row(HGNC ID,Approved Symbol)
false
==

As of version 2.0 there are two flags that can automate some of the interactions.

==
:- set_prolog_flag(bio_db_pl_from_zip, user).
:- set_prolog_flag(bio_db_del_zip, user).
==

In both cases the recognised values for the flags are: [user,true,false].
User is for prompting the user and true is progressing with an implicit yes answer.
The first flag automates conversion from .pl.zip to .pl (which will be the case
for the first time you access any dataset if you have installed bio_db_repo),
and the second controls the deletion of the zip file once the .pl file has been created.


Thanks to Jan Wielemaker for a retractall fix and for code for fast loading of precompiled fact bases
(and indeed for the changes in SWI that made this possible).

@author nicos angelopoulos
@version  0.5 2016/9/11
@version  0.7 2016/10/21,  experimenting with distros in github
@version  0.9 2017/3/10,   small changes for pack(requires) -> pack(lib) v1.1
@version  1.0 2017/10/9    to coincide with ppdp paper presentation
@version  2.1 2018/11/27   introduces cells and mouse data (and fixed dependency of 2.0)
@version  2.4 2019/4/2     test: bio_db_stats, new mouse db predicates, iface: bio_db_data_predicate/4
@see doc/Realeases.txt for version details.

*/

/** bio_db_paths.

    Initialisation call- setting up path aliases. 

    There are two main directory repositories the predicate 
    deals with: (a) the bio_db installed databases root (alias bio_db_data), and
    (b) the root of downloaded databases (alias bio_db_downloads).
    Optionally a top directory of which both (a) and (b) are subdirs can be defined (alias bio_db).
    The default value for alias bio_db is a made-up pack directory pack(bio_db_repo).
    The default for bio_db_data is sub directory =data= of alias bio_db, while
    bio_db_downloads defaults to sub directory =downloads= of the alias bio_db.
    The canonical subdirectory name for (a) is data and for (b) is downloads.

    pack(bio_db_repo) can also be installed as a complete package from SWI's manager.

    ==
    ?- pack_install( bio_db_repo ).
    ==

    This will install all the Prolog database files. The single tar and gzipped file is  
    246 Mb in size and the fully expanded version of a Prolog installation 
    can take up to 3.1Gb. The precise size depends on how many tables 
    are accessed at least once (each producing an expanded .pl and a .qlf file).

    Directory locations for (a) and (b) above can be given as either prolog flags with
    key bio_db_root and bio_dn_root respectively or via environment variables
    BioDbRoot and BioDnRoot. 

Installed root alias(bio_db_data) contains sub-dirs

  * graphs
    for graphs; string and reactome

  * maps 
    for all the supported maps

The above are mapped to aliases bio_graphs and bio_maps respectively.
Within each of these sub-directories there is further
structure based on the database the set was originated.
    
Downloaded root alias(bio_db_downloads) may contain sub-dirs
 
   * hgnc 
     data from HGNC database 

   * ncbi 
     data from NCBI database

   * reactome
     data from Reactome database

   * string 
     data from string database
    
   * uniprot 
     protein data from EBI

   * ense
     ensembl database

Alias bio_db_downloads is only useful if you are downloading data files directly from
the supported databases. 

See 
==
?- absolute_file_name( packs(bio_db(auxil)), Auxil ), ls( Auxil ).
==
for examples of how these can be used. 

For most users these aliases are not needed as the library manages them automatically.

@tbd transfer datasets and downloads to new pack location when running on newly installed
SWI version upgrade.

*/

bio_db_paths :-
    bio_db_paths_root,
    bio_db_paths_installed,
    bio_db_paths_installed_sub,
    bio_db_paths_downloaded.

bio_db_paths_root :-
    bio_db_setting( bio_db_root, Root ),
    !,
    bio_db_path_new( bio_db, Root ).
bio_db_paths_root.

bio_db_paths_installed :-
    bio_db_setting( bio_db_data_root, DbRoot ),
    !,
    bio_db_path_new( bio_db_data, DbRoot ).
bio_db_paths_installed :-
    user:file_search_path( bio_db, BioDb ),
    os_path_1( BioDb, data, BioDbData ),
    % exists_directory( DbRoot ),
    !,
    bio_db_path_new( bio_db_data, BioDbData ).
bio_db_paths_installed :-
    throw( missing_setting(bio_db_data_root) ).

bio_db_paths_downloaded :-
    bio_db_setting( bio_db_downloads_root, DnRoot ),
    !,
    bio_db_path_new( bio_db_downloads, DnRoot ).
bio_db_paths_downloaded :-
    user:file_search_path( bio_db_downloads_root, BioRoot ),
    os_path_1( BioRoot, downloads, DnRoot ),
    exists_directory( DnRoot ),
    !,
    bio_db_path_new( bio_db_downloads, DnRoot ).

bio_db_paths_installed_sub :-
    user:file_search_path( bio_db_data, DbRoot ),
    findall( Sub, bio_db_sub(Sub), Subs ),
    maplist( bio_db_paths_installed_sub(DbRoot), Subs ),
    !.

bio_db_paths_installed_sub( DbRoot, Sub ) :-
    os_path_1( DbRoot, Sub, AbsSub ),
    % exists_directory( Abs ),
    % directory_files( Abs, DbSubs ), % os_dirs
    ( atom_concat(SubSingular,'s',Sub) -> true; SubSingular = Sub ),
    atom_concat( bio_, SubSingular, BioDbSub ),
    bio_db_path_new( BioDbSub, AbsSub ),
    % os_path_1( AbsSub, Db, AbsDb )
    % bio_db_path_new( Db, AbsDb ),
    % bio_db_source( Sub, Db ),
    findall( DbSub, bio_db_source(Sub,DbSub), DbSubs ),
    maplist( bio_db_paths_installed_sub_dbs(AbsSub,Sub), DbSubs ),
    !.
bio_db_paths_installed_sub( _DbRoot, _Sub ).

bio_db_paths_installed_sub_dbs( Abs, Sub, Db ) :-
    bio_db_source( Sub, Db ),
    os_path_1( Abs, Db, Full ),
    bio_db_path_new( Db, Full ).
% bio_db_paths_installed_sub_dbs( _Abs, _Sub ).

bio_db_setting( PlSet, Value ) :-
    current_prolog_flag( PlSet, Value ),
    debug( bio_db, 'bio_db setting via flag: ~w, set to: ~w', [PlSet,Value] ),
    !.
bio_db_setting( PlSet, Value ) :-
    atomic_list_concat( Parts, '_', PlSet ),
    maplist( upcase_first, Parts, Arts ),
    atomic_list_concat( Arts, EnvVar ),
    getenv( EnvVar, Value ),
    debug( bio_db, 'bio_db setting via env: ~w, setting: ~w, set to: ~w', [EnvVar,PlSet,Value] ),
    !.
bio_db_setting( PlSet, Value ) :-
    bio_db_setting_default( PlSet, Value ).

bio_db_path_new( Alias, New ) :-
    user:file_search_path( Alias, Old ),
    bio_db_path_new_exists( Alias, Old, New ),
    !.
bio_db_path_new( Alias, Path ) :-
    debug( bio_db, 'Asserting search alias: ~w, to ~p', [Alias,Path] ),
    assert( user:file_search_path(Alias,Path) ).

bio_db_path_new_exists( _Alias, Old, Old ) :-
    !.
bio_db_path_new_exists( Alias, Old, New ) :-
    throw( fixme(alias_exists(Alias,Old,New)) ).

bio_db_path_exists( Alias ) :- % fixme: is this called from anywhere ?
    throw( fixme(bio_db_paths_installed/0,search_path_exists(Alias)) ).

upcase_first( Atom, Upped ) :-
    sub_atom( Atom, 0, 1, _, Flw ), 
    upcase_atom( Flw, Fup ),
    atom_length( Atom, Len ),
    Ken is Len - 1,
    sub_atom( atom, 1, Ken, 0, Tail ),
    atom_concat( Fup, Tail, Upped ).

/** bio_db_version( ?Vers, -Date ).

Version Mj:Mn:Fx, and release date date(Y,M,D).

==
?- bio_db_version( V, D ).
V = 2:4:0,
D = date(2019, 4, 2).
==

@see bio_db_data_predicate/4  (which should be generated for each new version)
@see doc/Releases.txt for more detail on change log

*/
% bio_db_version( 0:3:1, date(2015,7,25) ).
% bio_db_version( 0:4:0, date(2015,9,16) ).
% bio_db_version( 0:5:0, date(2016,9,10) ).
% bio_db_version( 0:6:0, date(2016,10,13) ).
% bio_db_version( 0:7:0, date(2016,11,21) ).   % experimenting with github distros
% bio_db_version( 0:8:0, date(2016,11,22) ).
% bio_db_version( 0:9:0, date(2017,03,10) ).   % beta for 1:0
% bio_db_version( 1:0:0, date(2017,10,08) ).   
% bio_db_version( 1:1:0, date(2017,10,13) ).   
% bio_db_version( 2:0:0, date(2018,11,23) ).   
% bio_db_version( 2:1:0, date(2018,11,27) ).   
% bio_db_version( 2:2:0, date(2018,12,6) ).   
% bio_db_version( 2:3:0, date(2019,2,11) ).   
bio_db_version( 2:4:0, date(2019,4,2) ).   

%% bio_db_citation( -Atom, -Bibterm ).
%
% This predicate succeeds once for each publication related to this library.
% Atom is the atom representation suitable for printing while Bibterm 
% is a bibtex(Type,Key,Pairs) term of the same publication. 
% Produces all related publications on backtracking.
%
%
bio_db_citation( Atom, bibtex(Type,Key,Pairs) ) :-
  Atom = 'Accessing biological data as Prolog facts.\nNicos Angelopoulos and Jan Wielemaker. In Proceedings of 19th International Symposium on Principles and Practice of Declarative Programming, Namur, Belgium, October, 2017 (PPDP\'17), 10 pages.',
  Type = inproceedings,
  Key  = 'AngelopoulosN_GiamasG_2015',
  Pairs = [
               title  = 'Accessing biological data as Prolog facts',
               author = 'Nicos Angelopoulos and Jan Wielemaker',
               booktitle= '19th International Symposium on Principles and Practice of Declarative Programming',
               year = 2017,
               month = 'October',
               address= 'Namur, Belgium'
               % url     = 'http://ceur-ws.org/Vol-1433/tc_74.pdf'
     ].

bio_db_citation( Atom, bibtex(Type,Key,Pairs) ) :-
  Atom = 'A logical approach to working with biological databases.\nNicos Angelopoulos and Georgios Giamas.\nProceedings of the 31st International Conference on Logic Programming (ICLP 2015) Accepted as a technical communication.\nCork, Ireland. September 2015.',
  Type = inproceedings,
  Key  = 'AngelopoulosN_GiamasG_2015',
  Pairs = [
               author = 'Nicos Angelopoulos and Georgios Giamas',
               title  = 'A logical approach to working with biological databases',
               booktitle= 'Technical Communication in Proceedings of the 31st International Conference on Logic Programming (ICLP 2015)',
               year = 2015,
               month = 'September',
            address= 'Cork, Ireland',
               url     = 'http://ceur-ws.org/Vol-1433/tc_74.pdf'
     ].

bio_db_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom = 'Working with biological databases.\nNicos Angelopoulos and Georgios Giamas.\n1th Workshop on Constraint Based Methods for Bioinformatics (2015)\nCork, Ireland. September 2015',
    Type    = inproceedings,
    Key  = 'AngelopoulosN_GiamasG_2015a',
    Pairs = [
               author = 'Nicos Angelopoulos and Georgios Giamas',
            title  = 'Working with biological databases',
            booktitle = '11th Workshop on Constraint Based Methods for Bioinformatics (2015)',
            year = 2015,
            month = 'September',
            address = 'Cork, Ireland',
            url = 'http://clp.dimi.uniud.it/wp/wp-content/uploads/2015/08/WCB_2015_paper_1.pdf'
    ].

/** bio_db_source( ?Type, ?Db ).

   True if Db is a source database for bio_db serving predicate of type Type.
   Type is either maps or graphs.

   The databases are 
   * hgnc
   * gont
   * ncbi
   * string
   * unip 

*/
bio_db_source( maps, hgnc ).
bio_db_source( maps, gont ).
bio_db_source( maps, unip ).
bio_db_source( maps, ncbi ).
bio_db_source( graphs, string ).
% bio_db_source( graphs, gont ). % these are used for aliases, so gont already exists
bio_db_source( graphs, reactome ).

bio_db_sub( graphs ).
bio_db_sub( maps ).

bio_db_setting_default( 'bio_db_root', BioDbRoot ) :-
    absolute_file_name( pack(bio_db), BioDb ),
    directory_file_path( Dir, bio_db, BioDb ),
    directory_file_path( Dir, bio_db_repo, BioDbRoot ).
bio_db_setting_default( 'bio_db_data_root', BioDbData ) :-
    absolute_file_name( bio_db(data), BioDbData ).
bio_db_setting_default( 'bio_db_downloads_root', BioDbDnloads ) :-
    absolute_file_name( bio_db(downloads), BioDbDnloads ).

%% bio_db_interface( ?Iface, -Status ).
%
% Interrogate the installation status (=|true|= or =|false|=) of bio_db's known interfaces.
% =|true|= if the interface dependencies are installed and the interface can be used,
% and =|false=| otherwise.
% 
% Can be used to enumerate all known or installed interfaces.
%
%==
% ?- findall( Iface, bio_db_interface(Iface,_), Ifaces ).
% Ifaces = [prolog, berkeley, prosqlite, rocks].
%==
% 
bio_db_interface( prolog, true ).
bio_db_interface( berkeley, Bool ) :-
    ( catch( use_module( library(bdb) ), _, fail ) -> Bool = true; Bool = false ).
bio_db_interface( prosqlite, Bool ) :-
    ( catch( use_module( library(prosqlite) ), _, fail ) -> Bool = true; Bool = false ).
bio_db_interface( rocks, Bool ) :-
    ( catch( use_module( library(rocksdb) ), _, fail ) -> Bool = true; Bool = false ).


%% bio_db_interface( ?Iface ).
%
% Interrogate or set the current interface for bio_db database predicates.
% By default =|Iface = prolog|=. Also supported: =|prosqlite|= (needs pack proSQLite),
% =|berkley|= (needs SWI's own library(bdb) and =|rocks|= (needs pack(rocskdb).
%
% == 
% ?- bio_db_interface( Iface ).
% Iface = prolog.
% 
% ?- debug( bio_db ).
% true.
% 
% ?- bio_db_interface( wrong ).
% % Could not set bio_db_interface prolog_flag, to: wrong, which in not one of: [prolog,prosqlite,berkeley,rocks]
% false.
% 
% ?- bio_db_interface( Iface ).
% Iface = prolog.
% 
% ?- map_hgnc_symb_hgnc( 'LMTK3', Hgnc ).
% % Loading prolog db: /usr/local/users/nicos/local/git/lib/swipl-7.1.32/pack/bio_db_repo/data/maps/hgnc/map_hgnc_symb_hgnc.pl
% Hgnc = 19295.
% 
% ?- bio_db_interface( prosqlite ).
% % Setting bio_db_interface prolog_flag, to: prosqlite
% true.
% 
% ?- map_hgnc_prev_symb( Prev, Symb ).
% % prosqlite DB:table hgnc:map_hgnc_prev_symb/2 is not installed, do you want to download (Y/n) ? 
% % Execution Aborted
% ?- map_hgnc_prev_symb( Prev, Symb ).
% % Loading prosqlite db: /usr/local/users/nicos/local/git/lib/swipl-7.1.32/pack/bio_db_repo/data/maps/hgnc/map_hgnc_prev_symb.sqlite
% Prev = 'A1BG-AS',
% Symb = 'A1BG-AS1' ;
%
% ==
% In which case Iface is prosqlite.
%
bio_db_interface( Iface ) :-
    var( Iface ),
    !,
    current_prolog_flag( bio_db_interface, IfacePrv ),
    bio_db_interface_known( IfacePrv, Iface ).
bio_db_interface( Iface ) :-
    ground( Iface ),
    bio_db_interface_set( Iface ).

bio_db_info( Iface, Pid, Key, Value ) :-
    var( Iface ),
    !,
    bio_db_info_gen( Iface, Pid, Key, Value ).
bio_db_info( Iface, Pid, Key, Value ) :-
    atom( Iface ),
    bio_db_info_source( Iface, Pid, Key, Value ).

bio_db_info_gen( Iface, Pid, Key, Value ) :-
    bio_db_interface_extensions( Iface, _ ),
    bio_db_info( Iface, Pid, Key, Value ).

bio_db_install_defaults( [org(hs),interactive(true)] ).

/** bio_db_install( +PidOrPname, +Iface ).
    bio_db_install( +PidOrPname, +Iface, +Opts ).

Install the interface (Iface) for bio_db database that corresponds to predicate identifier (Pid) 
or a predicate name (Pname). Note that this is not necessary to do in advance as the library 
will auto load missing Iface and Pid combinations when first interrogated.

Opts
  * interactive(Ictive=true)
    set false to accept default interactions
  * org(Org=hs)
    organism

*/
bio_db_install( PorP, Iface ) :-
    bio_db_install( PorP, Iface, [] ).
bio_db_install( PorP, Iface, OptS ) :-
    options_append( bio_db_install, OptS, Opts ),
    options( interactive(Ictive), Opts ),
    options( org(Org), Opts ),
    bio_db_porp_call( PorP, bio_db_install/2, Call ),
    bio_db_map_call_db_pname( Call, Db, Pname, Arity ),
    ( bio_db_info(Iface,PorP,_,_) -> 
        Mess = '~a DB:table ~w is already installed. It will be overwritten. Continue',
        Args = [Iface,PorP],
        ui_yes_no( Ictive, Mess, Args, y, Reply ), 
        ( Reply == true ->
            bio_db_interface_extensions( Iface, [Ext|_] ),
            ( bio_db_pname_source(Org,Db,Pname,read,Ext,File) ->
                delete_installed( Ext, File )
                ;
                true
            ),
            bio_db_serve_pname( false, false, Org, Db, Pname, Arity, Iface, Call )
            ;
            % ensure qlf is also installed, before failing
            ( Iface == prolog ->
                ( bio_db_pname_source(Org,Db,Pname,read,qlf,_ExistFile) ->
                    Mess1 = 'Qlf is also istalled.',
                    phrase('$messages':translate_message(debug(Mess1,[])), Lines1),
                    print_message_lines(current_output, kind(informational), Lines1)
                    ;
                    bio_db_pname_source( Org, Db, Pname, read, pl, File ),
                    bio_db_load_call( false, Pname, Arity, Iface, File, true )
                )
                ;
                true
            )
        )
        ;
        bio_db_serve_pname( false, false, Db, Pname, Arity, Iface, Call )
    ).

delete_installed( rocks, Dir ) :-
    delete_installed_db_dir_and_info( Dir ).
delete_installed( db, File ) :-
    delete_installed_db_file_and_info( File ).
delete_installed( sqlite, File ) :-
    delete_installed_db_file_and_info( File ).
delete_installed( pl, File ) :-
    delete_installed_db_file_and_info( File ),
    file_name_extension( Stem, _Ext, File ),
    file_name_extension( Stem,  qlf, Qile ),
    ( exists_file(Qile) ->
        debug( bio_db, 'Deleting file: ~p', Qile ),
        delete_file(Qile)
        ;
        true
    ).

delete_installed_db_file_and_info( File ) :-
    ( exists_file(File) ->
        debug( bio_db, 'Deleting file: ~p', File ),
        delete_file( File )
        ;
        true
    ),
    file_name_extension( Stem, Ext, File ),
    atom_concat( Stem, '_info', InfoStem ),
    file_name_extension( InfoStem, Ext, InfoFile ),
    ( exists_file(InfoFile) ->
        debug( bio_db, 'Deleting file: ~p', InfoFile ),
        delete_file( InfoFile )
        ;
        true
    ).

delete_installed_db_dir_and_info( Dir ) :-
    ( exists_directory(Dir) ->
        debug( bio_db, 'Deleting directory: ~p', Dir ),
        delete_directory_contents( Dir )
        ;
        true
    ),
    file_name_extension( Stem, Ext, Dir ),
    atom_concat( Stem, '_info', InfoStem ),
    file_name_extension( InfoStem, Ext, InfoDir ),
    ( exists_directory(InfoDir) ->
        debug( bio_db, 'Deleting directory: ~p', InfoDir ),
        delete_directory_contents( InfoDir )
        ;
        true
    ).

/** bio_db_porp_call( PorP, CallerId, Call ). 

    Constract a generic call from predicate id or predicate name (Porp).

*/
bio_db_porp_call( Porp, Cid, Call ) :-
    ground( Porp ),
    bio_db_porp_call_ground( Porp, Cid, Call ).

bio_db_porp_call_ground( Pname/Arity, _Cid, Call ) :- !,
    functor( Call, Pname, Arity ).
bio_db_porp_call_ground( Pname, Cid, Call ) :-
    % find the name from the module def of bio_db. A bit hackish.
    atom( Pname ),
    absolute_file_name( pack('bio_db/prolog/bio_db.pl'), BioDbF, [access(exist)] ),
    open( BioDbF, read, In ),
    read( In, ModuleDef ), 
    close( In ),
    ModuleDef = (:- module( bio_db, Pids ) ),
    ( memberchk(Pname/Arity,Pids) ->
        true
        ;
        Err = pack_error(not_a_db_pred(Pname),bio_db:Cid),
        throw( Err )
    ),
    functor( Call, Pname, Arity ).

/** bio_db_predicate_name( +PidOrPname, -Pname ).

Auxiliary predicate that strips the Pname of Pid or assumes
atomic PidOrPname to be a Pname. 

@tbd check it looks like a db name. this is only useful for db_preds.

*/
bio_db_predicate_name( Pid, Pname ) :-
    ground( Pid ),
    bio_db_predicate_name_ground( Pid, Pname ).

bio_db_predicate_name_ground( Pname/_, Pname ) :- !.
bio_db_predicate_name_ground( Pname, Pname ) :-
    atom( Pname ).

bio_db_predicate_db( Pid, Db ) :-
    bio_db_predicate_name( Pid, Pname ),
    atomic_list_concat( [_,Db|_], '_', Pname ).

bio_db_info_source( Iface, Pid, Key, Value ) :-
    bio_db_predicate_name( Pid, Pname ),
    bio_db_predicate_db( Pname, Db ),
    bio_db_pname_source( Db, Pname, read, Iface, DbF ),
    ( bio_db_interface_initialisation(Iface) -> true; true ),
    bio_db_info_db_file( Iface, Pid, DbF, Key, Value ).

bio_db_info_db_file( prolog, _Pid, DbF, Key, Value ) :-
    bio_db_pl_info( DbF, Infos ),
    member( Info, Infos ),
    arg( 1, Info, Key ),
    arg( 2, Info, Value ).
bio_db_info_db_file( prosqlite, Pid, DbF, Key, Value ) :-
    bio_db_predicate_info( Pid, Info ),
    % bio_db_source_info( DbF, InfoF ),
    sqlite_connect( DbF, Info ),
    atom_concat( 'Select * from ', Info, Query ),
    findall( Row, sqlite_query(Info,Query,Row), Rows ),
    sqlite_disconnect( Info ),
    member( row(Key,ValueAtom), Rows ),
    ( catch(atom_to_term(ValueAtom,Value,_),_,fail) ->
        true
        ;
        Value = ValueAtom
    ).
bio_db_info_db_file( berkeley, Pid, DbF, Key, Value ) :-
    bio_db_info_interface_infos( berkeley, Pid, DbF, _,  KVs ),
    member( Key-Value, KVs ).
bio_db_info_db_file( rocks, Pid, DbF, Key, Value ) :-
    bio_db_info_interface_infos( rocks, Pid, DbF, _,  KVs ),
    member( Key-Value, KVs ).

/** bio_db_info( +Pid, ?Iface ).
    bio_db_info( +Pid, ?Key, -Value ).
    bio_db_info( +Iface, +Pid, ?Key, -Value ).

Retrieve information about bio_db database predicates.

When Iface is not given, Key and Value are those of the interface under which Pid 
is currently open for access. The predicate errors if Pid is not open for serving yet.

The bio_db_info/2 version succeeds for all interfaces Pid is installed- it is simply
a shortcut to: =|bio_db_info( Iface, Pid, _, _ )|=.

The Key-Value information returned are about the particular data predicate
as saved in the specific backend.

Key

  * source_url
   an atomic value of the URL

  * datetime
   datetime/6 term

  * data_types
    data_types/n given the primary type for each argyument in the data table

  * header
   row/n term, where n is the number of columns in the data table

  * unique_lengths
   unique_lengths/3 term, lengths for the ordered sets of: Ks, Vs and KVs

  * relation_type(From,TO)  
    where From and To take values in 1 and m

==
?- bio_db_info( Iface, map_hgnc_hgnc_symb/2, Key, Value), write( Iface:Key:Value ), nl, fail.
prolog:source_url:ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz
prolog:datetime:datetime(2016,9,10,0,2,14)
prolog:data_types:data_types(integer,atom)
prolog:unique_lengths:unique_lengths(44266,44266,44266)
prolog:relation_type:relation_type(1,1)
prolog:header:row(HGNC ID,Approved Symbol)
prosqlite:source_url:ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz
prosqlite:datetime:datetime(2016,9,10,0,2,14)
prosqlite:data_types:data_types(integer,atom)
prosqlite:unique_lengths:unique_lengths(44266,44266,44266)
prosqlite:relation_type:relation_type(1,1)
prosqlite:header:row(HGNC ID,Approved Symbol)
==

*/
bio_db_info( PorP, Iface ) :-
    bio_db_info( Iface, PorP, _, _ ),
    !.

bio_db_info( Pid, Key, Value ) :-
    bio_db_db_predicate( Pid ),
    !,
    bio_db_info_pred( Pid, Key, Value ).
bio_db_info( Pid, _Key, _Value ) :-
    Err = pack_error(bio_db,bio_db_info/3,not_a_db_pred(Pid)),
    throw( Err ).

bio_db_info_pred( Pid, Key, Value ) :-
    bio_db_handle( Pid, Iface, File, Handle, _Mod ),
    !,
    bio_db_info_interface( Iface, Pid, File, Handle, Key, Value ).

bio_db_info_pred( Pid, _Key, _Value ) :-
    Err = pack_error(bio_db,bio_db_info/3,close_to_info(Pid)),
    throw( Err ).

bio_db_info_interface_kvs( Iface, Pid, File, Handle, KVs ) :-
    bio_db_info_interface_infos( Iface, Pid, File, Handle, Pairs ),
    \+ var( KVs ),
    bio_db_info_interface_kvs( KVs, Pairs ).
    
bio_db_info_interface_kvs( [], _ ).
bio_db_info_interface_kvs( [K-V|T], Pairs ) :-
    memberchk( K-V, Pairs ),
    bio_db_info_interface_kvs( T, Pairs ).
    
bio_db_info_interface( Iface, Pid, File, Handle, Key, Value ) :-
    bio_db_info_interface_infos( Iface, Pid, File, Handle, KVs ),
    member( Key-Value, KVs ).
    
bio_db_info_interface_infos( Callable, Pid, _File, _Handle, Pairs ) :-
    memberchk( Callable, [prolog,prosqlite] ),
    !,
    bio_db_predicate_info( Pid, InfoName ),
    Goal =.. [InfoName,Key,Value],
    findall( Key-Value, ( (Key = interface, Value = Callable) ;  bio_db:Goal ), Pairs ).
bio_db_info_interface_infos( berkeley, _Pid, File, _Handle, KVs ) :-
    % fixme add key = Berkley interface
    % ( ((Key=interface, Value=berkeley); bdb_enum( Handle, info+Key , Value)) ).
    bio_db_source_info( File, InfoF ),

    bdb_open( InfoF, read, InfoHandle, [key(atom),value(term)] ),
    findall( AKey-AValue, bdb_enum(InfoHandle,AKey,AValue), Pairs ),
    bdb_close( InfoHandle ),
    KVs = [interface-berkeley|Pairs].
bio_db_info_interface_infos( rocks, _Pid, File, _Handle, KVs ) :-
    % fixme add key = Berkley interface
    file_name_extension( Stem, Ext, File ),
    atom_concat( Stem, '_info', InfoStem ),
    file_name_extension( InfoStem, Ext, InfoFile ),
    rocks_open( InfoFile, InfoHandle, [key(atom),value(term)] ),
    findall( AKey-AValue, rocks_enum(InfoHandle,AKey,AValue), Pairs ),
    rocks_close( InfoHandle ),
    KVs = [interface-rocks|Pairs].

/** bio_db_close( +Pid ).

    Close the current serving of predicate Pid.
    Next time a Pid Goal is called the current interface (bio_db_interface/1)
    will be used to establish a new server and resolve the query.

    Predicate throws an error if the Pid does not correspond to a db_predicate
    or if it is not currently servered by any of the backends.

===
?- bio_db_interface( prosqlite ).
?- map_hgnc_hgnc_symb( Hgnc, Symb ).
Hgnc = 506,
Symb = 'ANT3~withdrawn' .

?- bio_db_close( map_hgnc_hgnc_symb/2 ).
?- bio_db_interface( prolog ).
?- map_hgnc_hgnc_symb( Hgnc, Symb ).
Hgnc = 1,
Symb = 'A12M1~withdrawn' .
?- bio_db_close( map_hgnc_hgnc_symb/2 ).

===
    
*/
bio_db_close( Pid ) :-
    bio_db_db_predicate( Pid ),
    !,
    bio_db_close_pred( Pid ).
bio_db_close( Pid ) :-
    Err = pack_error(not_a_db_pred(Pid),bio_db:bio_db_close/1),
    throw( Err ).

bio_db_close_pred( Pid ) :-
    bio_db_handle( Pid, Iface, File, Handle, Mod ),
    !,
    bio_db_close_connection( Iface, Handle ),
    Pid = Pname/Arity,
    functor( Head, Pname, Arity ),
    retractall( Head ),
    atom_concat( Pname, '_info', InfoPname ),
    functor( InfoHead, InfoPname, 2 ),
    retractall( InfoHead ),
    retractall( bio_db_handle(Pid,Iface,File,Handle,Mod) ),
    assert( (Head :- bio_db_serve(Head)) ).
bio_db_close_pred( Pid ) :-
    % debug( bio_db, 'No server for predicate: ~w, found', Pid ),
    Err = pack_error(bio_db,bio_db_close/1,not_served(Pid)),
    throw( Err ),
    fail.

bio_db_close_connection( prosqlite, Handle ) :-
    sqlite_disconnect( Handle ).
bio_db_close_connection( prolog, _Handle ).
bio_db_close_connection( berkeley, Handle ) :- 
    bdb_close( Handle ).
bio_db_close_connection( rocks, Handle ) :- 
    rocks_close( Handle ).

/** bio_db_close_connections.

Close all currently open bio_db backend connections.

This is called by bio_db at halt.

*/
bio_db_close_connections:-
    bio_db:bio_db_handle( Pid, _B, _C, _D, _Mod ),
    bio_db_close( Pid ),
    fail.
bio_db_close_connections.

/** bio_db_db_predicate( ?Pid ).

    True if Pid is a predicate identifier which is defined in current bio_db session,
    and starts with either edge_ or map_. When Pid is a free variable
    all such predicate identifiers are returned on backtracking.

    For a statically produced list of all data predicates in _bio_db_
    see, bio_db_data_predicate/4.

==
  ?- bio_db_db_predicate( map_hgnc_hgnc_symb/2 ).
  true.

  ?- bio_db_db_predicate( X ).
  X = map_hgnc_symb_entz/2 ;
  X = map_ense_enst_ensg/2 ;
  ...
==

*/
bio_db_db_predicate( Pname/Arity) :-
    ground(Pname/Arity), !,
    functor(Head,Pname,Arity),
    bio_db_predicate_name(Pname),
    % predicate_property(bio_db:Head, exported), !.
    predicate_property(bio_db:Head, defined), !.  
    % fixme: when called from closing,  maybe do a bit of checking ? \+ (rule=:=1,clauses=:=1)
bio_db_db_predicate( Pname/Arity) :-
    % module_property(bio_db, exports(List)),
    % member(Pname/Arity, List),
    current_predicate( bio_db:Pname/Arity ),
    bio_db_predicate_name(Pname).

bio_db_predicate_name(Pname) :-
    (   sub_atom(Pname, 0, _, _, map_)
    ->  true
    ;   sub_atom(Pname, 0, _, _, edge_)
    ).

% map stubs, 
% these are in memory iff the map is to be loaded as prolog 
% and this is the first call to the pred, they get replaced
% by the map data after that.
% 
bio_db_serve( Call ) :-
    functor( Call, Pn, _ ),
    ( ( atomic_list_concat([_,_,Org,_,_],'_',Pn),
        bio_db_organism(Org)
       ) ->
            true
            ; 
            ( (atomic_list_concat([_,_,Org|_],'_',Pn),
                 bio_db_organism(Org)
              ) ->
                    true
                    ;
                    % fixme: it is probably better to abort here...
                    Org = hs
       )
    ),
    bio_db_serve( Org, Call, true ).

bio_db_serve( Org, Call ) :-
    bio_db_serve( Org, Call, true ).

bio_db_serve( Org, Call, Load ) :-
    bio_db_interface( Iface ),
    bio_db_map_call_db_pname( Call, Db, Pname, Arity ),
    bio_db_serve_pname( Load, true, Org, Db, Pname, Arity, Iface, Call ).

bio_db_interface_set( Iface ) :-
    bio_db_interface_atom( Iface ),
    !,
    M = 'Setting bio_db_interface prolog_flag, to: ~a',
    debug( bio_db, M, Iface ),
    ( bio_db_interface_initialisation(Iface) -> true; true ),
    set_prolog_flag( bio_db_interface, Iface ).
bio_db_interface_set( Iface ) :-
    findall( Aface, bio_db_interface_atom(Aface), AllFaces ),
    Err = pack_error(bio_db,bio_db_interface/2,arg_enumerate(1,AllFaces,Iface) ),
    throw( Err ).
    /*
    M = 'Could not set bio_db_interface prolog_flag, to: ~w, which in not one of: ~w',
    findall( Face, bio_db_interface_atom(Face), Faces ),
    debug( true ),
    debug( true, M, [Iface,Faces] ), % fixme: this is warning rather than debug
    debug( true ),
    fail.
    */
    
bio_db_interface_extensions( prolog, [pl,''] ).
bio_db_interface_extensions( prosqlite, [sqlite,''] ).
bio_db_interface_extensions( berkeley, [db,''] ).
bio_db_interface_extensions( rocks, [rocks,''] ).

bio_db_interface_known( Prov, Iface ) :-
    atomic( Prov ),
    bio_db_interface_atom( Prov ),
    !,
    Iface = Prov.
bio_db_interface_known( Prov, Def ) :-
    bio_db_default_interface( Def ),
    M = 'Resetting bogus bio_db_interface prolog_flag, from: ~w, to default: ~a',
    debug( bio_db, M, [Prov,Def] ), % fixme: this is informational rather than debug
    set_prolog_flag( bio_db_interface, Def ).

% prosqlite here
/*
bio_db_serve_pname( load, Db, Pname, Arity, Call ) :-
    current_prolog_flag( bio_db_interface, prosqlite ),
    !,
    Term =.. [Db,Pname],
    absolute_file_name( Term, Src, [access(Mode),file_type(prolog),file_errors(fail)] ).
    sqlite_connect( phones, phones_db, as_predicates(true) )
    */

/** bio_db_serve_pname( +LoadFlag, +Ictive, Org, Db, Pname, Arity, Iface, _Call ).

LoadFlag can be one of check, true (for loading) and false for ensuring
the db is installed but does not actually hot-swap it in. Ictive is a boolean
with true for interactively questioning user whereas false accepts the defaults
with no interupptions.

*/
bio_db_serve_pname( check, _Ictive, Org, Db, Pname, _Arity, Iface, _Call ) :-
    !,
    % bio_db_interface_extensions( Iface, Exts ),
    bio_db_interface_extensions( Iface, [Ext|_] ),
    % new implementation, untested:
    bio_db_pname_source( Org, Db, Pname, read, Ext, _Abs ).
    % % bio_db_db_pname_source( Db, Pname, exist, Ext, Abs ),
    % Rel =.. [Db|Pname],
    % absolute_file_name( Rel, Abs, [extensions(Exts),access(exist)] ),
    % exists_file( Abs ),

bio_db_serve_pname( Load, _Ictive, Org, Db, Pname, Arity, Iface, Call ) :-
    bio_db_interface_extensions( Iface, [Ext|_] ),
    bio_db_pname_source( Org, Db, Pname, read, Ext, File ),
    % bio_db_db_pname_source( Db, Pname, exist, Ext, Load ),
    % user:file_search_path( Db, _DbPath ),
    !,
    bio_db_load_call( Load, Pname, Arity, Iface, File, Call ).
bio_db_serve_pname( Load, Ictive, Org, Db, Pname, Arity, Iface, Call ) :-
    Iface \== prolog,
    bio_db_interface_extensions( prolog, [Ext|_] ),
    bio_db_pname_source( Org, Db, Pname, read, Ext, File ),
    Mess = '~a DB:table ~w:~w is not installed, but the Prolog db exists. Shall it be created from Prolog',
    Args = [Iface,Db,Pname/Arity],
    ui_yes_no( Ictive, Mess, Args, y, Reply ),
    Reply == true,
    % bio_db_serve_pname_from_local( Reply, Db, Pname, Arity, Iface, Load, Call ),
    bio_db_pl_nonpl_interface( Iface, File, NonPlLoad ),
    !,
    % fixme: add logic for deleting prolog interface of downloaded db
    bio_db_load_call( Load, Pname, Arity, Iface, NonPlLoad, Call ).
bio_db_serve_pname( Load, Ictive, Org, Db, Pname, Arity, Iface, Call ) :-
    % bio_db_pname_source( Db, Pname, read, prolog+zip, ZLoad ),
    % bio_db_pname_source( Db, Pname, read, 'pl.zip', ZLoad ),
    bio_db_pname_source( Org, Db, Pname, read, prolog+zip, ZLoad ),
    !,
    file_name_extension( PlLoad, zip, ZLoad ),
    current_prolog_flag( bio_db_pl_from_zip, PlFromZipFlag ),
    ( PlFromZipFlag == user ->
        Mess = '~a DB:table ~w:~w is not installed, but the zipped prolog db exists. Shall it be created from this',
        Args = [Iface,Db,Pname/Arity],
        ui_yes_no( Ictive, Mess, Args, y, Reply )
        ;
        MessFg = '~a DB:table ~w:~w is not installed, but the zipped prolog db exists. Flag bio_db_pl_from_zip says: ~w',
        message_report( MessFg, [Iface,Db,Pname/Arity,PlFromZipFlag], informational ),
        Reply = PlFromZipFlag
    ),
    ( Reply == true ->
        file_directory_name( ZLoad, Dir ),
        archive_extract( ZLoad, Dir, [] ),
        ( Iface \== prolog ->
            bio_db_pl_nonpl_interface( Iface, PlLoad, NonPlLoad ),
            bio_db_reply_delete_file( true, PlLoad )
            ;
            current_prolog_flag(bio_db_del_zip,DelZipFlag),
            ( DelZipFlag == user ->
                ZipDelMess = 'Delete the zip file: ~p',
                ui_yes_no( Ictive, ZipDelMess, [ZLoad], n, ZipDelReply )
                ;
                MessDelFg = 'Zip file will be deleted depending on value of flag bio_db_del_zip, which is: ~w',
                message_report( MessDelFg, [DelZipFlag], informational ),
                ZipDelReply = DelZipFlag
            ),
            bio_db_reply_delete_file( ZipDelReply, ZLoad ),
            NonPlLoad = PlLoad
        ),
        !,
        bio_db_load_call( Load, Pname, Arity, Iface, NonPlLoad, Call )
        ;
        % fixme: do fresh download
        debug( bio_db, 'Downloading fresh zip file for: ~w', Pname/Arity ),
        delete_file( ZLoad ),
        file_directory_name( ZLoad, DataDir ),
        directory_files( DataDir, DataFiles ),
        findall( Delable-FullDel, ( member(Delable,DataFiles), 
                                file_name_extension(Pname,_DelExt,Delable), 
                                directory_file_path(DataDir,Delable,FullDel)
                            ),
                                Delables ),
        maplist( bio_db_conflict_file, Delables ),
        bio_db_serve_pname_reply( true, Ictive, Load, Org, Db, Pname, Arity, Iface, Call )
    ).
% here  fixem: 
% add logic that warns if other interfaces will be 
bio_db_serve_pname( Load, Ictive, Org, Db, Pname, Arity, Iface, Call ) :-
    ( Iface == prolog -> 
        Mess = '~a DB:table ~w:~w is not installed, do you want to download it'
        ;
        Mess = '~a DB:table ~w:~w is not installed, do you want to download the prolog db and then generate this interface'
    ),
    Args = [Iface,Db,Pname/Arity],
    ui_yes_no( Ictive, Mess, Args, y, Reply ),
    bio_db_serve_pname_reply( Reply, Ictive, Load, Org, Db, Pname, Arity, Iface, Call ).

bio_db_serve_pname_reply( false, _Ictive, _Load, _Org, _Db, _Pname, _Arity, _Iface, _Call ) :-
    abort.
bio_db_serve_pname_reply( true, Ictive, Load, Org, Db, Pname, Arity, Iface, Call ) :-
    stoics( Stoics ),
    Mess = 'Downloading dataset from server: ~w',
    phrase('$messages':translate_message(debug(Mess,[Stoics])), Lines),
    print_message_lines(current_output, kind(informational), Lines),
    atomic_list_concat( [PredType|_], '_', Pname ), 
    bio_db_predicate_type_sub_dir( PredType, Sub ),
    atomic_list_concat( [Stoics,Org,Sub,Db,Pname], '/', StoicsStem ),
    atomic_list_concat( [StoicsStem,pl,zip], '.', StoicsFile ),
    bio_db_pname_source( Org, Db, Pname, none, 'pl.zip', Local ),
    debug( bio_db, 'Trying to get: ~w', url_file(StoicsFile,Local) ),
    % directory_file_path( LocDir, _, Local ),
    file_directory_name( Local, LocalDir ),
    % here
    bio_db_repo_skeleton_pack,
    make_directory_path( LocalDir ),
    url_file( StoicsFile, Local ),
    % fixme: delete the .pl file here if it exists before unpacking ?  % although this is inconsistent with calling logic
    archive_extract( Local, LocalDir, [] ),
    % here( 'Unzip the pl, create the Iface and if not Iface==Prolog, suggest deleting the .pl db' ),
    file_name_extension( LocalPlF, zip, Local ),
    directory_files( LocalDir, LocalFiles ),
    bio_db_interface_extensions( Iface, [Ext|_] ),
    findall( Delable-FullDel, ( member(Delable,LocalFiles), 
                            file_name_extension(Pname,DelExt,Delable), 
                            \+ memberchk(DelExt,['pl.zip',pl,Ext]),
                            directory_file_path(LocalDir,Delable,FullDel)
                            ),
                                Delables ),
    debug( bio_db, 'Candidates for deletion: ~w', [Delables] ),

    ( \+ exists_file(LocalPlF) -> 
        throw( decompression_didnot_produce(LocalPlF) )
        ; 
        % here: ask to delete .zip file
        ZipDelMess = 'Delete the zip file: ~p',
        ui_yes_no( Ictive, ZipDelMess, [Local], n, ZipDelReply ),
        bio_db_reply_delete_file( ZipDelReply, Local )
    ),
    ( Iface == prolog ->
        NonPlLoad = LocalPlF
        ;
        bio_db_pl_nonpl_interface( Iface, LocalPlF, NonPlLoad ),
        PlDelMess = 'Delete the Prolog file: ~p',
        ui_yes_no( Ictive, PlDelMess, [LocalPlF], y, PlDelReply ),
        bio_db_reply_delete_file( PlDelReply, LocalPlF )
    ),
    maplist( bio_db_conflict_file, Delables ),
    % then( 'go back and make sure you deal with existing other interfaces (delete them)' ),
    !,
    bio_db_load_call( Load, Pname, Arity, Iface, NonPlLoad, Call ).
    % we probably (now need something lighter than:
    % bio_db_serve_pname( load, Db, Pname, Arity, Iface, Call ).

bio_db_repo_skeleton_pack :-
    absolute_file_name( pack(bio_db), BioDbD, [file_type(directory)] ),
    directory_file_path( PackD, _, BioDbD ),
    directory_file_path( PackD, bio_db_repo, RepoD ),
    directory_file_path( RepoD, 'pack.pl', RepoPackPl ),
    ( exists_file(RepoPackPl) ->
        true
        ;
        make_directory_path( RepoD ),
        ensure_loaded( pack('bio_db/auxil/lib/bio_db_repo_info') ),
        findall( InfTerm, bio_db_repo_info(InfTerm), [InfNm,InfTi|Infs] ),
        date_two_digit_dotted( Dotted ),
        atomic_list_concat( [YrA,MnA,DyA], '.', Dotted ),
        % atomic_list_concat( [Dotted,skeleton], '-', PlPackVers ),
        Clauses = [InfNm,InfTi,version(Dotted)|Infs],
        portray_clauses( Clauses, file(RepoPackPl) ),
        atomic_list_concat( [20,YrA], FullYA ),
        maplist( atom_number, [YrA,FullYA,MnA,DyA], [Yr,FullY,Mn,Dy] ), % the day gets a -skeleton suffix
        atomic_list_concat( [DyA,skeleton], '-', DyPsfx ),
        directory_file_path( RepoD, prolog, RepoPlD ),
        make_directory_path( RepoPlD ),
        directory_file_path( RepoPlD, 'bio_db_repo_version.pl', ModVersF ),
        portray_clauses( [bio_db_repo_version(Yr:Mn:DyPsfx,date(FullY,Mn,Dy))], file(ModVersF) ),
        directory_file_path( BioDbD, 'auxil/lib/bio_db_repo.pl', BioDbRepoPlF ),
        directory_file_path( RepoPlD, 'bio_db_repo.pl', DstRepoF ),
        copy_file( BioDbRepoPlF, DstRepoF )
    ).

bio_db_conflict_file( Delable-Full ) :-
    Mess = 'Current db file might be inconsistent to new zip file. Delete db file: ~p',
    Ictive = false,
    % fixme: should we be passing Ictive from above ?
    ui_yes_no( Ictive, Mess, [Delable], y, Reply ),
    bio_db_reply_delete_file( Reply, Full ).

/*
bio_db_serve_pname_from_local( false, _Db, _Pname, Arity,Iface, Load, Call ) :-
    ( bio_db_db_pname_source( Db, Pname, read, prolog+zip, ZLoad ) ->
        fail  % .zip will be tried by caller on failure
        ; 
    ).
    fail.
    */
% fixme: this is not called from anywhere? 
bio_db_serve_pname_from_local( true, _Db, Pname, Arity, Iface, Load, Call ) :-
    % fixme: add predicates for interogating and deleting db/interface pairs
    bio_db_pl_nonpl_interface( Iface, Load, NonPlLoad ),
    % fixme: add logic for deleting prolog interface of downloaded db
    !,
    bio_db_load_call( Pname, Arity, Iface, NonPlLoad, Call ).

bio_db_pl_nonpl_interface( Iface, Load, NonPlLoad ) :-
    debug( bio_db, 'Converting to interface: ~a, from file: ~p', [Iface,Load] ),
    atom_concat( pl_, Iface, Stem ),
    atom_concat( 'bio_db/auxil/backends/', Stem, Backend ),
    ensure_loaded( pack(Backend) ),
    Conv =.. [Stem,Load],
    call( Conv ),
    file_name_extension( LoadStem, _Pl, Load ),
    bio_db_interface_extensions( Iface, [Ext|_] ),
    file_name_extension( LoadStem, Ext, NonPlLoad ).

bio_db_ensure_loaded( Iface, Pid, Load, Handle, From ) :-
    atom( Iface ),
    bio_db_ensure_loaded_1( Iface, Pid, Load, Handle, From ),
    !.
bio_db_ensure_loaded( Iface, Pid, Load, _Handle, _From ) :-
    % fixme: Goal in error can be supplied ?
    Err = pack_error(bio_db,bio_db_ensure_loaded/4,failed_to_load(Iface,Pid,Load) ),
    throw( Err ).

bio_db_ensure_loaded_1( prolog, Pid, Load, [], From ) :-
    Pid = Pname/_Arity,
    atomic_list_concat( [Ppfx|_], '_', Pname ),
    bio_db_pl_load( Ppfx, Pid, Load, From ).
bio_db_ensure_loaded_1( prosqlite, Pname/_Arity, Load, Pname, _From ) :-
    sqlite_connect( Load, Pname, [as_predicates(true),at_module(bio_db)] ).
bio_db_ensure_loaded_1( berkeley, Pname/Arity, Load, Berkeley, _From ) :-
    \+ '$bio_db_handle'(Pname,_),
    % fixme: is the option needed ? we are just reading- check
    % bio_db_info_interface( berkeley, _Pid, Load, _Handle, data_types, data_types(Ktype,Vtype) ),

    Pairs = [data_types-DtTypes,relation_type-RelType],
    bio_db_info_interface_kvs( berkeley, _Pid, Load, _Handle, Pairs ),
    bio_db_info_interface_types( RelType, DtTypes, berkeley, Dup, _DbTypes, KeyType, ValType ),
    % Open = bdb_open( Load, read, Berkeley, [duplicates(Dupl),key(KeyType),value(ValType)] ),
    Open = bdb_open( Load, read, Berkeley, [dup(Dup),key(KeyType),value(ValType)] ),
    debug( bio_db, 'Bdb opening for reading with: ~w' , Open ),
    call( Open ),
    % bdb_open( Load, read, Berkeley, [duplicates(true),key(KeyType),value(ValType)] ),  % 0.5
    % retractall( '$bio_db_handle'(Pname,_) ),  % fixme: we can do some error reporting if something does exist
    % assert( '$bio_db_handle'(Pname,Berkeley) ),
    % atomic_list_concat( [Ppfx|_], '_', Pname ),
    arg( 1, RelType, Krt ),
    arg( 1, RelType, Vrt ),
    ground( Arity ),
    bio_db_berkeley_predicate_assert_arity( Arity, Krt, Vrt, Pname, bdb_get, bdb_enum, Berkeley ).
bio_db_ensure_loaded_1( rocks, Pname/Arity, Load, Handle, _From ) :-
    /*
    bio_db_info_interface( rocks, _Pid, Load, _Handle, data_types, data_types(Ktype,Vtype) ),
    */
    Pairs = [data_types-DtTypes,relation_type-RelType],
    bio_db_info_interface_kvs( rocks, _Pid, Load, _Handle, Pairs ),
    bio_db_info_interface_types( RelType, DtTypes, rocks, Dup, _DbTypes, KeyType, ValType ),
    % maplist( bio_db_info_rocks_singleton_type, [Ktype,Vtype], [Kbype,Vbype] ),
    % ( Dup == false -> KeyType = NoDupKeyType; NoDupKeyType = term ),
    % 2nd take, duplicates are now stored as lists of values
    ( Dup == false -> ValType = DupValType; DupValType = term ),
    Open = rocks_open( Load, Handle, [key(KeyType),value(DupValType)] ),
    debug( bio_db, 'Rocks opening for reading with: ~w' , Open ),
    call( Open ),

    % atomic_list_concat( [Ppfx|_], '_', Pname ),
    bio_db_rocks_predicate_assert_arity( Arity, Dup, Pname, rocks_get, rocks_enum, Handle ).
    % bio_db_rocks_predicate_assert_arity( Kbype/Vbype, Arity, Pname, rocks_get, rocks_enum, Handle ).

% bio_db_pl_load( map, Pid, Load, From ).
bio_db_pl_load( _Type, Pid, Load, Mod ) :-
    dynamic( Mod:Pid ),  % fixme: we should be able to remove this? 
    % ensure_loaded( Load ).  % following is an elaboration of code by JW: 16.11.13:
    (   (file_name_extension(Base,pl,Load), \+ current_prolog_flag(bio_db_qcompile,false))
    ->  Mod:load_files( Base, [qcompile(auto),if(not_loaded)] )
    ;   ensure_loaded( Mod:Load )  % fixme: use load_files/2 ?
    ).

% bio_db_pl_load( edge, Pname/_Arity, Load ) :-
/*
bio_db_pl_load( edge, Pid, Load ) :-
    % os_postfix ... :(
    % % file_name_extension( Base, Ext, Load ),
    % % atomic_list_concat( [Base,ord], '_', OrdBase ),
    % % file_name_extension( OrdBase, Ext, OrdLoad ),
    % % ensure_loaded( OrdLoad ),
    ensure_loaded( Load ),
    % % atomic_list_concat( [Pname,ord], '_', Pord ),
    % % Head =.. [Pname,X,Y,W],
    % % GoalF =.. [Pord,X,Y,W],
    % % GoalB =.. [Pord,Y,X,W],
    % % consult_clause( (Head:-(GoalF;GoalB)) ).
    true.
    */

    /*
bio_db_kv_db_predicate_assert( _, Pname, Krt, Vrt, Arity, Get, Enum, Handle ) :-
    ground( Arity ),
    bio_db_kv_db_predicate_assert_arity( Arity, Krt, Vrt, Pname, Get, Enum, Handle ).
bio_db_kv_db_predicate_assert( edge, Pname, Arity, Get, Enum, Handle ) :-
    bio_db_kv_db_predicate_assert_edge( Arity, Pname, Get, Enum, Handle ).
    */

bio_db_berkeley_predicate_assert_arity( 2, 1, 1,  Pname, Get, Enum, Handle ) :-
    !, % maybe this relevat to other modes too  (here mode is 2,1,1
    Head =.. [Pname,Key,Value],
    GetG  =.. [  Get, Handle, Key, Value ],
    EnumG =.. [ Enum, Handle, Key, Value ],
    Conditional =  ( ( ground(Key) -> 
                    GetG
                    ;
                    EnumG
                  )
            ),
    consult_clause( (Head:-(Conditional)) ).

bio_db_berkeley_predicate_assert_arity( N, _, _,  Pname, Get, Enum, Handle ) :-
    functor( Head, Pname, N ),
    Head =.. [Pname,Key|Args],
    GetG  =.. [  Get, Handle, Key, Value ],
    EnumG =.. [ Enum, Handle, Key, Value ],
    Conditional =  ( ( ground(Key) -> 
                    GetG
                    ;
                    EnumG
                  )
            ),
    Unravel = bio_db_kv_db_value( Args, Value ),
    consult_clause( (Head:-(Conditional,Unravel)) ).

bio_db_rocks_predicate_assert_arity( 2, false, Pname, Get, Enum, Handle ) :-
    !, % maybe this relevat to other modes too  (here mode is 2, false (=no duplicates)
    Head =.. [Pname,Key,Value],
    GetG  =.. [  Get, Handle, Key, Value ],
    EnumG =.. [ Enum, Handle, Key, Value ],
    Conditional =  ( ( ground(Key) -> 
                    GetG
                    ;
                    EnumG
                  )
            ),
    consult_clause( (Head:-(Conditional)) ).
bio_db_rocks_predicate_assert_arity( N, false, Pname, Get, Enum, Handle ) :-
    N > 2,
    functor( Head, Pname, N ),
    Head =.. [Pname,Key|Args],
    GetG  =.. [  Get, Handle, Key, Value ],
    EnumG =.. [ Enum, Handle, Key, Value ],
    Conditional =  ( ( ground(Key) -> 
                    GetG
                    ;
                    EnumG
                  )
            ),
    Unravel = bio_db_kv_db_value( Args, Value ),
    consult_clause( (Head:-(Conditional,Unravel)) ).
bio_db_rocks_predicate_assert_arity( 2, true, Pname, Get, Enum, Handle ) :-
    !, % maybe this relevat to other modes too  (here mode is 2, false (=no duplicates)
    Head =.. [Pname,Key,Value],
    GetG  =.. [  Get, Handle, Key, Values ],
    EnumG =.. [ Enum, Handle, Key, Values ],
    Conditional =  ( ( ground(Key) -> 
                    (GetG, bio_db_rocks_multi_key_value(Values,Value) )
                    ;
                    (EnumG, bio_db_rocks_multi_key_value(Values,Value) )
                  )
            ),
    consult_clause( (Head:-(Conditional)) ).
bio_db_rocks_predicate_assert_arity( Arity, true, Pname, Get, Enum, Handle ) :-
    Arity > 2,
    functor( Head, Pname, Arity ),
    Head =.. [Pname,Key|Args],
    GetG  =.. [  Get, Handle, Key, ValueTerm ],
    EnumG =.. [ Enum, Handle, Key, ValueTerm ],
    % EnuTG =.. [ Enum, Handle, Key:_X, Value ],
    Conditional =  ( ( ground(Key) -> 
                    ( GetG, bio_db_rocks_multi_key_value(ValueTerm,Value) )
                    ;
                    ( EnumG, bio_db_rocks_multi_key_value(ValueTerm,Value) )
                    % ( EnumG , ( (atomic(ProvKey),ProvKey=Key);ProvKey=Key:_) )
                  )
                ),
    Unravel = bio_db_kv_db_value( Args, Value ),
    consult_clause( (Head:-(Conditional,Unravel)) ).
    

bio_db_rocks_multi_key_value( [H|T], Value ) :-
    !,
    ( Value = H; member( Value, T ) ).
bio_db_rocks_multi_key_value( Value, Value ).

bio_db_kv_db_value( [H], Value ) :- !, Value = H.
bio_db_kv_db_value( [H|T], H+Value ) :-
    bio_db_kv_db_value( T, Value ).
consult_clause( Clause ) :-
    assert( Clause ).

/*
consult_clause( Clause ) :-
    tmp_file_stream(text, File, Stream),
    portray_clause( Stream, Clause ),
    close( Stream ),
    debug( bio_db, 'Consulting from: ~p', File ),
    consult( File ),
    true.
*/

bio_db_interfaces_ext( A+B, Ext ) :-
    !,
    bio_db_interfaces_ext( A, AExt ),
    bio_db_interfaces_ext( B, BExt ),
    atomic_list_concat( [AExt,BExt], '.', Ext ).
bio_db_interfaces_ext( Iface, Ext ) :-
    bio_db_interface_extensions( Iface, [Ext|_] ),
    !.
bio_db_interfaces_ext( Ext, Ext ).

bio_db_pname_source( _Org, Db, Pname, Mode, DbFaces, Src ) :-
    % fixme: make it play with Org ?
    bio_db_interfaces_ext( DbFaces, Ext ),
    Term =.. [Db,Pname],
    debug( bio_db, 'Trying DB location: ~p, mode: ~w', [Term,Mode] ),
    ( absolute_file_name( Term, Src, [access(Mode),extensions([Ext]),file_errors(fail)] )
      ;
      (  DbFaces==rocks,
         file_name_extension(Pname,rocks,Rname),
        Rerm =.. [Db,Rname],
        absolute_file_name(Rerm,Src,[access(Mode),file_errors(fail),file_type(directory)])
      )
    ),
    !.
% The above is a short-cut this is the long way.
% Works when single db provides both maps and graphs
%
bio_db_pname_source( Org, Db, Pname, Mode, DbFaces, Src ) :-
    bio_db_interfaces_ext( DbFaces, Ext ),
    % Term =.. [Db,Pname],
    bio_db_pred_name_type( Pname, Type ),
    directory_file_path( Org, Type, Rel ),
    % Term =.. [bio_db_data,Type], % pre Org times
    Term =.. [bio_db_data,Rel],
    absolute_file_name( Term, Dir ),
    file_name_extension( Pname, Ext, Bname ),
    directory_file_path( Dir, Db, DbDir ),
    directory_file_path( DbDir, Bname, Src ),
    debug( bio_db, 'Trying DB location: ~p, mode: ~w', [Src,Mode] ), % fixme: debug_call, with success/failure
    ( absolute_file_name( Src, _, [access(Mode),file_errors(fail)])
       ;
      (  DbFaces==rocks,
        absolute_file_name( Src, _, [access(Mode),file_errors(fail),file_type(directory)] )
      )
    ),
    !.
    % absolute_file_name( Pname, Src, [access(Mode),extensions([Ext]),file_errors(fail)] ).

/** bio_db_source_info( +File, -InfoF ).

    Generate Info filename corresponding to the database filename at File.

*/
bio_db_source_info( File, InfoF ) :-
    file_name_extension( Stem, Ext, File ),
    atom_concat( Stem, '_info', InfoStem ),
    file_name_extension( InfoStem, Ext, InfoF ).

/** bio_db_predicate_info( +PidOrPname, -InfoName ).

    Generate the information predicate name of a Pid or of Db predicate name.

*/
bio_db_predicate_info( Pname/_Arity, InfoName ) :-
    !,
    atom_concat( Pname, '_info', InfoName ).
bio_db_predicate_info( Pname, InfoName ) :-
    atom( Pname ),
    atom_concat( Pname, '_info', InfoName ).

bio_db_pred_name_type( Pname, Type ) :-
    atomic_list_concat( [Fst|_], '_', Pname ),
    bio_db_pred_name_prefix_type( Fst, Type ).

bio_db_pred_name_prefix_type( edge, graphs ).
bio_db_pred_name_prefix_type( map, maps ).

bio_db_load_call( false, Pname, Arity, Iface, File, _Call ) :-
    ( Iface == prolog -> 
        % ensure .qlf is created
        file_name_extension( Stem, pl, File ),
        Mess = 'Ensuring .qlf is also installed: ~w',
        phrase('$messages':translate_message(debug(Mess,[Pname/Arity])), Lines),
        print_message_lines(current_output, kind(informational), Lines),
        load_files( scratch:Stem, [qcompile(auto),if(true)] ),
        abolish( scratch:Pname/Arity )
        ;
        true
    ).
bio_db_load_call( true, Pname, Arity, Iface, File, Call ) :-
    debug( bio_db, 'Loading pred: ~w, interface: ~a, file: ~w', [Pname/Arity,Iface,File] ),
    ground( Iface ),
    functor( Phead, Pname, Arity ),
    ( predicate_property(Phead,imported_from(From) ) -> true; From = bio_db ),
    abolish( From:Pname/Arity ),    % fixme: retractall/1 if we have problem with regenerations ?
    % retractall(Phead),
    atom_concat( Pname, '_info', InfoPname ),
    dynamic( From:InfoPname/2 ),
    % functor( Ihead, InfoPname, 2 ),
    ( (From \== bio_db,\+ current_predicate(bio_db:InfoPname/2)) -> 
            % fixme: test again:
            From:export(InfoPname/2),
            bio_db:import(From:InfoPname/2)
            ;
            true
    ),
    functor( InfoHead, InfoPname, 2),
    retractall( From:InfoHead ),
    bio_db_ensure_loaded( Iface, Pname/Arity, File, Handle, From ),
    assert( bio_db_handle(Pname/Arity,Iface,File,Handle,From) ),
    call( Call ).

bio_db_predicate_type_sub_dir( map, maps ).
bio_db_predicate_type_sub_dir( edge, graphs ).

bio_db_map_call_db_pname( Call, Db, Pname, Arity ) :-
    functor( Call, Pname, Arity ),
    at_con( [Type,Db|_], '_', Pname ),
    bio_db_type_arity_check( Type, Arity ).

bio_db_type_arity_check( Type, Arity ) :-
    bio_db_type_arity_known( Type, Arity ),
    !.
bio_db_type_arity_check( Type, Arity ) :-
    throw( unknown_combination_of_type_arity(Type,Arity) ).

% fixme: this now a bit outdated... maybe add name for special cases ?
bio_db_type_arity_known( map, 2 ).
bio_db_type_arity_known( map, 3 ).
bio_db_type_arity_known( map, 5 ).
bio_db_type_arity_known( map, 7 ).
bio_db_type_arity_known( edge, 3 ).
bio_db_type_arity_known( edge, 2 ).

bio_db_reply_delete_file( true, Local ) :-
    debug( bio_db, 'Deleting file: ~p', Local ),
    delete_file( Local ).
bio_db_reply_delete_file( false, Local ) :-
    debug( bio_db, 'NOT deleting file: ~p', Local ).

/** go_term_symbols( +GoT, -Symbols, -Opts ).

Gets the symbols belonging to a GO term. Descents to GO child relations, 
which by default are includes (reverse of is_a) and consists_of (reverse of part_of)
to pick up Symbols recursively.

FIXME: changing edge_--- to map_ ----

==
Opts 
  * descent(Desc=true)
    whether to collect symbols from descendant GO terms
  * as_child_includes(Inc=true)
    collect from edge_gont_include/2
  * as_child_consists_of(Cns=true)
    collect from edge_gont_consists_of/2
  * as_child_regulates(Reg=false)
    collect from edge_gont_regulates/2
  * as_child_negatively_regulates(Reg=false)
    collect from edge_gont_negatively_regulates/2
  * as_child_positively_regulates(Reg=false)
    collect from edge_gont_positively_regulates/2
  * debug(Dbg=false)
    see options_append/3

Listens to debug(go_term_symbols).

go_term_symbols( 'GO:0000375', Symbs ).

Symbs = [ALYREF,AQR,ARC,BCAS2,BUD13,BUD31,C7orf55-LUC7L2,CACTIN,CCAR1,CD2BP2,CDC40,CDC5L,CDK13,CELF1,CELF2,CELF3,CELF4,CLNS1A,CLP1,CPSF1,CPSF2,CPSF3,CPSF7,CRNKL1,CSTF1,CSTF2,CSTF3,CTNNBL1,CWC15,CWC22,CWC27,DBR1,DCPS,DDX1,DDX17,DDX20,DDX23,DDX39A,DDX39B,DDX41,DDX46,DDX5,DGCR14,DHX15,DHX16,DHX32,DHX35,DHX38,DHX8,DHX9,DNAJC8,DQX1,EFTUD2,EIF4A3,FRG1,FUS,GCFC2,GEMIN2,GEMIN4,GEMIN5,GEMIN6,GEMIN7,GEMIN8,GPATCH1,GTF2F1,GTF2F2,HNRNPA0,HNRNPA1,HNRNPA2B1,HNRNPA3,HNRNPC,HNRNPD,HNRNPF,HNRNPH1,HNRNPH2,HNRNPH3,HNRNPK,HNRNPL,HNRNPM,HNRNPR,HNRNPU,HNRNPUL1,HSPA8,ISY1,KHSRP,LSM1,LSM2,LSM3,LSM6,LSM7,LSM8,LUC7L,LUC7L2,LUC7L3,MAGOH,MBNL1,METTL14,METTL3,MPHOSPH10,NCBP1,NCBP2,NCBP2L,NHP2L1,NOL3,NOVA1,NUDT21,PABPC1,PABPN1,PAPOLA,PAPOLB,PCBP1,PCBP2,PCF11,PHAX,PHF5A,PLRG1,PNN,POLR2A,POLR2B,POLR2C,POLR2D,POLR2E,POLR2F,POLR2G,POLR2H,POLR2I,POLR2J,POLR2K,POLR2L,PPIE,PPIH,PPIL1,PPIL3,PPWD1,PQBP1,PRMT5,PRMT7,PRPF19,PRPF3,PRPF31,PRPF4,PRPF4B,PRPF6,PRPF8,PSIP1,PTBP1,PTBP2,RALY,RBM17,RBM22,RBM5,RBM8A,RBMX,RBMXP1,RNPS1,RSRC1,SAP130,SART1,SART3,SCAF11,SETX,SF1,SF3A1,SF3A2,SF3A3,SF3B1,SF3B2,SF3B3,SF3B4,SF3B5,SF3B6,SFPQ,SFSWAP,SKIV2L2,SLU7,SMC1A,SMN1,SMN2,SMNDC1,SNRNP200,SNRNP40,SNRNP70,SNRPA,SNRPA1,SNRPB,SNRPB2,SNRPC,SNRPD1,SNRPD2,SNRPD3,SNRPE,SNRPF,SNRPG,SNRPGP15,SNUPN,SNW1,SRPK2,SRRM1,SRRM2,SRSF1,SRSF10,SRSF11,SRSF12,SRSF2,SRSF3,SRSF4,SRSF5,SRSF6,SRSF7,SRSF9,STRAP,SUGP1,SYF2,SYNCRIP,TDRD12,TFIP11,TGS1,TRA2A,TRA2B,TXNL4A,TXNL4B,U2AF1,U2AF2,UBL5,UPF3B,USP39,USP4,USP49,WBP4,WDR77,WDR83,XAB2,YBX1,YTHDC1,ZCCHC8,ZRSR2]
==

@author nicos angelopoulos
@version  0.1 2015/7/26

*/

/** symbols_string_graph( +Symbols, -Graph, +Opts ).

Create the string database Graph between Symbols.

Opts 
  * minw(0)
    minimum weight (0 =< x =< 999) - not checked

  * sort_pairs(Spairs=true)
    set to false to leave order of edges dependant on order of Symbols

  * include_orphans(Orph=true)
    set false to exclude orphans from Graph

  * sort_graph(Sort=true)
    set to false for not sorting the results

==
?- Gont = 'GO:0043552', findall( Symb, map_gont_gont_symb(Gont,Symb), Symbs ),
   symbols_string_graph( Symbs, Graph, [] ),
   length( Graph, Len ).
   
==
@author nicos angelopoulos
@version  0.1 2016/01/18

*/


/*
bio_db_info_db_types( berkeley, RelType, DataTypes, Dup, DbTypes, KeyType, ValType ) :-
    bio_db_info_berkeley_types( RelType, DataTypes, Dup, DbTypes, KeyType, ValType ).
bio_db_info_db_types( rocks, RelType, DataTypes, Dup, DbTypes, KeyType, ValType ) :-
    % bio_db_info_rocks_types( RelType, DataTypes, Dup, DbTypes, KeyType, ValType ).
    bio_db_info_rocks_types( RelType, DataTypes, Dup, DbTypes, KeyType, ValType ).

bio_db_info_rocks_types( relation_type(1,1), DataTypes, Dup, DbTypes, KeyType, ValType ) :- 
    DataTypes =.. [data_types,PlKeyType,PlValsTypes],
    bio_db_info_rocks_type( PlKeyType, KeyType ),
    bio_db_info_rocks_type( PlValsTypes, ValType ),
    DbTypes = [key(KeyType),value(ValType)].
    */
% fixme: change all the calls and remove this
bio_db_info_db_types( Iface, RelType, DataTypes, Dup, DbTypes, KeyType, ValType ) :-
    bio_db_info_interface_types( RelType, DataTypes, Iface, Dup, DbTypes, KeyType, ValType ).

% bio_db_info_berkeley_types( relation_type(1,MR), data_types(Kt,Vt), Dup, DbTypes, KeyType, ValType ) :-   
bio_db_info_interface_types( relation_type(1,MR), data_types(Kt,Vt), Iface, Dup, DbTypes, KeyType, ValType ) :- 
    ( MR =:= 1 -> Dup = false; Dup = true ),
    !,  % Arity = 2 (from the form of data_types...
    bio_db_info_interface_type( Kt, Iface, KeyType ),
    bio_db_info_interface_type( Vt, Iface, ValType ),
    DbTypes = [key(KeyType),value(ValType)].
bio_db_info_interface_types( relation_type(1,MR), DtTypes, Iface, Dup, DbTypes, KeyType, ValType ) :-   
    ( MR =:= 1 -> Dup = false; Dup = true ),
    !,  % Arity = 2 (from the form of data_types...
    functor( DtTypes, _, Arity ),
    Arity > 2,
    !,
    arg( 1, DtTypes, Kt ),
    bio_db_info_interface_type( Kt, Iface, KeyType ),
    ValType = term,
    DbTypes = [key(KeyType),value(term)].
bio_db_info_interface_types( RelType, DtTypes, Iface, Dup, DbTypes, KeyType, ValType ) :-
    ( RelType = relation_type(1,1) -> Dup = false; Dup = true ),
    arg( 1, DtTypes, Kt ),
    functor( DtTypes, _, Arity ),
    ( Arity > 2 -> ValType = term
                ; 

                arg( 2, DtTypes, Vt ),
                    bio_db_info_interface_type( Vt, Iface, ValType )
    ),
    bio_db_info_interface_type( Kt, Iface, KeyType ),
    DbTypes = [key(KeyType),value(term)].

bio_db_info_interface_type( [Singleton], Iface, Type ) :-  !,
    bio_db_info_interface_unit_type( Iface, Singleton, Type ).
bio_db_info_interface_type( [_,_|_], _Iface, term ) :- !.  % a bit of a shortcut
bio_db_info_interface_type( Singleton, Iface, Type ) :-
    bio_db_info_interface_unit_type( Iface, Singleton, Type ).

bio_db_info_interface_unit_type( berkeley, Unit, Type ) :-
    bio_db_berkeley_type( Unit, Type ).
bio_db_info_interface_unit_type( rocks, Unit, Type ) :-
    bio_db_rocks_type( Unit, Type ).

bio_db_rocks_type( term, term ).
bio_db_rocks_type( atom, atom ).
bio_db_rocks_type( integer, int64 ). % rocks also has int32
bio_db_rocks_type( number, atom ).  % rocks has doubles and floats

bio_db_berkeley_type( term, term ).
bio_db_berkeley_type( atom, atom ).
bio_db_berkeley_type( integer, c_long ).
bio_db_berkeley_type( number, atom ).

% this is a mock implementation see library(os) or library(os_) 
% for the real one
os_path_( Dir, File, Path ) :-
    ground( Dir ),
    ground( File ),
    !,
    directory_file_path( Dir, File, Path ).
os_path_1( Dir, File, Path ) :-
    ground( Path ),
    directory_file_path( DirSl, File, Path ),
    atom_concat( Dir, '/', DirSl ).
    
pack_errors:message( close_to_info(Pid) ) -->
    ['Predicate: ~w, is not currently served, info depend on the opening interface.'-[Pid]].
pack_errors:message( not_a_db_pred(Pid) ) -->
    ['Predicate identifier: ~w, not of a db predicate.'-[Pid]].
pack_errors:message( not_served(Pid) ) -->
    ['Predicate: ~w, is not currently served.'-[Pid]].
pack_errors:message( failed_to_load(Iface,Pid,File) ) -->
    ['Failed to load predicate: ~w, for backend: ~w, from file: ~p.'-[Pid,Iface,File]].

% add at_halt, close databases particularly berkeley ones
:- at_halt( bio_db_close_connections ).
:- initialization( bio_db_paths, after_load ).

:- multifile sandbox:safe_primitive/1.

bio_sandbox_clause(sandbox:safe_primitive(bio_db:Head)) :-
    module_property(bio_db, exports(PIList)),
    member(Name/Arity, PIList),
    (   sub_atom(Name, 0, _, _, edge_)
    ;   sub_atom(Name, 0, _, _, map_)
    ),
    functor(Head, Name, Arity).

term_expansion(bio_db_interface, Clauses) :-
    findall(Clause, bio_sandbox_clause(Clause), Clauses).

bio_db_interface.
sandbox:safe_primitive(bio_db:bio_db_info(_,_,_)).
sandbox:safe_primitive(bio_db:bio_db_info(_,_,_,_)).
sandbox:safe_primitive(bio_db:go_term_symbols(_,_,_)).
sandbox:safe_primitive(bio_db:symbols_string_graph(_,_,_)).
