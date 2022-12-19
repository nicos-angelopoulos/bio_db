
:- use_module(library(filesex)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:portray_clauses/2).
% :- lib(stoics_lib:url_file/3).
:- lib(stoics_lib:io_lines/2).

% also sets lib alias that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(de_semi/3).
:- lib(sep_split/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2
:- lib(url_file_local_date_mirror/3).
:- lib(map_list_options/3).

true(_,_).

std_gallus_maps_cgnc_defaults( Defs ) :-
    Defs = [
                    debug(true),
                    db_dir(cgnc),  % is this used ?
                    download(true),
                    map_prefix(true),
                    prefix(cgnc_gallus),
                    sub(maps)
           ].

/** std_gallus_maps_cgnc( +Opts ).

Create ID maps from CGNC's main download data file.

Input is the tab delimited file containing all CGNC public data. 
The fields in the file are: CGNC ID, Entrez Gene ID, Ensembl Gene ID,
gene symbol, gene name, gene synonym, curation status and last edit date.

In the interest of completeness we create a map for each column based on CGNC ID (cgnc).

Tokens
  * cgnc('CGNC id')
    chicken gene nomenclature consortium
  * entz('Entrez')
    Entrez gene ids 
  * ensg('Gene id')
    Ensembl gene ids
  * symb('gene symbol')
    Gene symobls
  * name('gene name')
    Gene, long, name.
  * syno('gene synonym')
    Gene synonym(s) - check syntax for multies
  * curs('curation status')
    Curation status (should be a factor)
  * edat('last edit date')
    should be a date data type

Opts
  * debug(Dbg=true)
    progress, informational messages
  * db_dir(DbDir=cgnc)
    relative directory within downloads and data to work in
  * sub(Dir=maps)
    sub-directory for creating the maps
  * download(Dn=true)
    set to false to skip downloading a fresh copy of the CGNC file(s)
  * map_prefix(Mfx=true)
    whether to include the prefix in the predicate name, passed to csv_ids_map/6
  * prefix(Pfx=cgnc)
    prefix to include, passed to csv_ids_map/6

==
?- std_gallus_maps_cgnc.
?- cd( '$HOME/.local/share/swi-prolog/pack/Downloads/bio_db-22.12.17/maps/cgnc' ).
?- shell( 'wc -l *' ).
==

@author nicos angelopoulos
@version  0.1 2022/12/17,  from hgnc
@tbd convert to url_..._mirror.pl
@see http://birdgenenames.org/cgnc/downloads.jsp?file=standard
*/
std_gallus_maps_cgnc :-
    std_gallus_maps_cgnc( [] ).

std_gallus_maps_cgnc( Args ) :-
    Self = std_gallus_maps_cgnc,
    CsvF = 'cgnc_complete_set.txt',
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    options( db_dir(RelDir), Opts ),
    absolute_file_name( bio_db_build_downloads(RelDir), Dir ),
    os_make_path( Dir, debug(true) ),
    options( download(Dnload), Opts ),
    os_path( Dir, CsvF, CsvP ),
    cgnc_download_file( Dnload, Self, SrcUrl, CsvP, [dir(Dir)|Opts] ),
    working_directory( Old, Dir ),
    os_ext( dnt, CsvF, DntF ),
    bio_db_dnt_times( DntF, DnDt, _DnEnd ),
    options( sub(SubDir), Opts ),
    make_directory_path( SubDir ),
    mtx( CsvF, Mtx, sep(tab) ),
    options_propagate( [map_prefix,prefix], Opts, StdOT, true ),
    StdO= [dir(SubDir),cnm_transform(cgnc_cname)|StdOT],
    Cgnc = 'CGNC id',
    Symb = 'gene symbol',
    Entz = 'Entrez Gene id',
    Ensg = 'Ensembl Gene id',
    Name = 'gene name',
    Syno = 'gene synonym',
    Curs = 'curation status',
    Edat = 'last edit date',

    cgnc_std_map( Cgnc, Symb, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, SymbF ),    % cgnc_symb
    cgnc_std_map( Cgnc, Entz, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, EntzF ),    % cgnc_entz
    cgnc_std_map( Cgnc, Ensg, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, EnsgF ),    % cgnc_ensg
    cgnc_std_map( Cgnc, Name, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, NameF ),    % cgnc_name
    cgnc_std_map( Cgnc, Syno, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, SynoF ),    % cgnc_syno
    cgnc_std_map( Cgnc, Curs, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, CursF ),    % cgnc_curs
    cgnc_std_map( Cgnc, Edat, CsvF, Mtx, Self, StdO, SrcUrl/DnDt, EdatF ),    % cgnc_edat
    debuc( Self, 'doing links...', [] ),
    Files = [SymbF,EntzF,EnsgF,NameF,SynoF,CursF,EdatF],
    % maplist( link_to_bio_sub(RelDir), Files ),
    map_list_options( link_to_bio_sub(RelDir), Files, call_options([org(gallus),type(maps)]) ),
    % file_name_extension( TxtF, gz, GzF ),
    % delete_file( TxtF ),
    working_directory( _, Old ).

cgnc_std_map( Cid1, Cid2, CsvF, Csv, Self, StdO, SrcUrl/DnDt, OutF ) :-
    cgnc_std_column_to_value_call( Cid1, Call1 ),
    cgnc_std_column_to_value_call( Cid2, Call2 ),
    Opts = [to_value_1(Call1),to_value_2(Call2)|StdO],
    % debuc( std_maps_hgnc, 'doing file for columns: ~w, ~w', [Cid1, Cid2] ),
    csv_ids_map( CsvF, Cid1, Cid2, Csv, OutF, [source(SrcUrl),datetime(DnDt)|Opts] ),
    debuc( Self, 'deposited on: ~w (columns: ~w, ~w)', [OutF,Cid1,Cid2] ).

cgnc_std_column_to_value_call( 'CGNC id', pos_integer ). % check they are unique and present (key) % de_semi('CGNC') ).
cgnc_std_column_to_value_call( 'Entrez Gene id', pos_integer ).
cgnc_std_column_to_value_call( 'Ensembl Gene id', non_empty_atom ).   % fixme: prefixed
cgnc_std_column_to_value_call( 'gene symbol', non_empty_atom ).
cgnc_std_column_to_value_call( 'gene name', non_empty_atom ).
cgnc_std_column_to_value_call( 'gene synonym', sep_split('|') ).      % fixme: check for multis
cgnc_std_column_to_value_call( 'curation status', non_empty_atom ).  
cgnc_std_column_to_value_call( 'last edit date', non_empty_atom ).    % fixme: a date
      % fixme: prefixed ENSG
% hgnc_std_column_to_value_call( 'prev_symbol', sep_split('|') ).
% hgnc_std_column_to_value_call( 'Chromosome', non_empty_atom ).   % old
% hgnc_std_column_to_value_call( 'Entrez Gene ID (supplied by NCBI)', pos_integer ).
% hgnc_std_column_to_value_call( 'Ensembl ID + supplied by Ensembl', non_empty_atom ). 
% hgnc_std_column_to_value_call( 'Entrez Gene ID + supplied by NCBI', pos_integer ).
% hgnc_std_column_to_value_call( 'hgnc_id', de_semi('HGNC') ).
% hgnc_std_column_to_value_call( 'symbol', non_empty_atom ).
% hgnc_std_column_to_value_call( 'name', non_empty_atom ). % old
% hgnc_std_column_to_value_call( 'entrez_id', pos_integer ).  % old
% hgnc_std_column_to_value_call( 'CCDS IDs', non_empty_atom ).  % old
% hgnc_std_column_to_value_call( 'ccds_id', non_empty_atom ). 
    
cohese_ensebl_gene_id( Dom, _Subo, Gid ) :-
    Dom \== '', 
    !,
    Gid = Dom.
cohese_ensebl_gene_id( _Dom, Subo, Gid ) :-
    Subo \== '', 
    !,
    Gid = Subo.
cohese_ensebl_gene_id( _Dom, _Subo, '' ).

cohese_gene_id( Dom, _Subo, Gid ) :-
    number( Dom ),
    Dom > 0,
    !,
    Gid = Dom.
cohese_gene_id( _Dom, Subo, Gid ) :-
    number( Subo ),
    Subo > 0, 
    !,
    Gid = Subo.
cohese_gene_id( _Dom, _Subo, '' ).
    
pos_integer( Numb, Numb ) :-
    integer( Numb ),
    Numb > 0.

non_empty_atom( Other, NonEmpty ) :-
    Other \== '',
    NonEmpty = Other.

cgnc_cname( A, B ) :-
    cgnc_cname_known( A, B ),
    !.
cgnc_cname( A, A ).

% cmpl = complement = both currated by HGNC and supplied by respective database.
% 
% CGNC id	Entrez Gene id	 Ensembl Gene id gene symbol	gene name	gene synonym	curation status	last edit date
cgnc_cname_known( 'CGNC id', cgnc ).
cgnc_cname_known( 'Entrez Gene id', entz ).
cgnc_cname_known( 'Ensembl Gene id', ensg ).
cgnc_cname_known( 'gene symbol', symb ).
cgnc_cname_known( 'gene name', name ).
cgnc_cname_known( 'gene synonym', syno ).
cgnc_cname_known( 'curation status', curs ).
cgnc_cname_known( 'last edit date', edat ).

cgnc_download_file( true, Self, Url, Dst, Opts ) :-
     Url = 'http://birdgenenames.org/cgnc/downloads.jsp?file=standard',
     options( dir(Dir), Opts ),
     url_file_local_date_mirror( Url, Dir, [file(Dst),date(prefix)|Opts] ),
     % url_file( Url, Dst, [dnt(true)|Opts] ),
     cgnc_download_file_fix( Self, Dst ).
cgnc_download_file( false, Self, Url, Dst, _Opts ) :-
     debuc( Self, 'Asked not to download: ~p', [Dst] ),
     Url = 'http://birdgenenames.org/cgnc/downloads.jsp?file=standard'.

cgnc_download_file_fix( Self, Dst ) :-
     io_lines( Dst, Lines ),
     debuc( Self, length, cgnc_dnload/Lines ),
     Lines = [[13],BCs|Tines],
     atom_codes( B, BCs ),
     at_con( BParts, '\t', B ),
     % debuc( Self, enum, cgnc_2nd_line_parts/BParts ),
     BParts = ['CGNC id','Entrez Gene id','gene symbol','gene name','gene synonym','curation status','last edit date'],
     BParts = [CGNC,Entz,Symb,Name,Syno,Curs,Edat],
     !,
     NewBParts = [CGNC,Entz,'Ensembl Gene id',Symb,Name,Syno,Curs,Edat],
     at_con( NewBParts, '\t', NewB ),
     atom_codes( NewB, NewBCs ),
     io_lines( Dst, [NewBCs|Tines] ).
cgnc_download_file_fix( Self, Dst ) :-
     debuc( Self, 'CGNC download seems in good order: ~p', [Dst] ).