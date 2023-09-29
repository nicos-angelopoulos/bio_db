
:- use_module(library(filesex)).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
% :- lib(bio_db).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:portray_clauses/2).

% also sets lib alias that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(de_semi/3).
:- lib(sep_split/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_add_infos/1).                  % bio_db_add_infos_to/2
:- lib(bio_db_dnt_times/3).
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(url_file_local_date_mirror/3).

true(_,_).

% Good on 2017/03/03.
% Comprehensive overhaul, as location of source file its contents and header names, had changed...
%    see src/std_hs-19.02.08.old.pl for the old version
%
std_maps_hgnc_defaults( Defs ) :-
    % absolute_file_name( bio_db_build_downloads(hgnc), Dir ),
    % expand_file_name( '$local/../work/db/data/hgnc', [Exp] ),
    % absolute_file_name( Exp, Dir ),
                              Defs = [  db(hgnc),
                                        debug(true), 
                                        debug_fetch(true),
                                        debug_url(false),
                                        download(true),
                                        hgnc_file('hgnc_complete_set.txt'),
                                        iactive(true),
                                        org(human)
                                     ].

/** std_maps_hgnc( +Opts ).

Create some maps from HGNC's "complete" data file.

Opts
 * db(hgnc)
   the source database
 * debug(Dbg=true)
   whether the session is interactive, otherwise wget gets --no-verbose
 * debug_fetch(Fbg=true)
   whether to debug the fetching of the url (via url_file_local_date_mirror/3)
 * debug_url(Ubg=false)
   whether to debug the concatenation of the url (via bio_db_source_url/3)
 * dir(Dir=maps)
   sub-directory for creating the maps
 * hgnc_file('hgnc_complete_set.txt')
   file for HGNC downloads
 * iactive(Iact=true)
   informational, progress messages
 * org(Org=human)
   organism

Opts are passed to url_file_local_date_mirror/3.

==
?- std_maps_hgnc.
?- cd( '$local/../work/db/maps/hgnc' ).
?- shell( 'wc -l hgnc_id*' ).
==

@author nicos angelopoulos
@version  0.1 2014/7/2
@version  0.2 2015/3/18,   added db based prefix
@version  0.3 2019/2/8,    accommodate the changes to the location and format of the file at the source
@version  0.4 2023/9/22,   move download components to options

*/
std_maps_hgnc :-
    std_maps_hgnc( [] ).

std_maps_hgnc( Args ) :-
    Self = std_maps_hgnc,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    build_dnload_loc( Self, DnDir, Opts ),
    bio_db_source_url( SrcUrl, [hgnc_file-url_file,debug_url-debug], Opts ),
    options( debug_fetch(Fbg), Opts ),
    url_file_local_date_mirror( SrcUrl, DnDir, [dnld_file(UrlF),debug(Fbg)|Opts] ),
    working_directory( Old, DnDir ),
    % HgncTxtF = 'hgnc_complete_set.txt',
    % GzF = 'hgnc_complete_set.txt.gz',
    % @ gunzip(-f,-k, GzF ),
    % bio_db_dnt_times( 'hgnc_complete_set.txt.gz.dnt', DnDt, _DnEnd ),
    bio_db_dnt_times( 'hgnc_complete_set.txt.dnt', DnDt, _DnEnd ),
    SubDir = maps,
    make_directory_path( SubDir ),

    options_propagate( map_prefix, Opts, StdOT, true ),
    % StdO= [dir(SubDir),cnm_transform(hgnc_cname)|StdOT],
    StdO= [dir(SubDir)|StdOT],
    Entz = 'entrez_id',
    Ensg = 'ensembl_gene_id',
    Hgnc = 'hgnc_id',
    Symb = 'symbol',
    Name = 'name',
    Prev = 'prev_symbol',
    Syno = 'alias_symbol',
    Chrm = 'location',
    Ccds =  'ccds_id',

    csv_ids_rows( UrlF, '\t', Csv ),
    hgnc_std_map( Hgnc, Symb, UrlF, Csv, StdO, SrcUrl/DnDt, HgncF ),               % hgnc_symb

    % fixme allow hgnc_std_map's called predicate to deal with multiple entries from single row
    % also it needs to be told not to sort some maps (but with ability to check uniqueness
    % hgnc_extra_symbols_column( Csv, 'alias_symbol', map_hgnc_syno_symb, SrcUrl/DnDt, SynoF ),
    % hgnc_extra_symbols_column( Csv, 'prev_symbol', map_hgnc_prev_symb, SrcUrl/DnDt, PrevF ),
    hgnc_std_map( Syno, Symb, UrlF, Csv, StdO, SrcUrl/DnDt, SynoF ),               % hgnc_name
    hgnc_std_map( Prev, Symb, UrlF, Csv, StdO, SrcUrl/DnDt, PrevF ),               % hgnc_name

    hgnc_std_map( Hgnc, Name, UrlF, Csv, StdO, SrcUrl/DnDt, HgncNameF ),               % hgnc_name
    hgnc_std_map( Symb, Hgnc, UrlF, Csv, StdO, SrcUrl/DnDt, SymbF ),               % symb_hgnc
    hgnc_std_map( Entz, Symb, UrlF, Csv, StdO, SrcUrl/DnDt, EntzF ),               % entz_symb
    hgnc_std_map( Symb, Entz, UrlF, Csv, StdO, SrcUrl/DnDt, SymbEntzF ),               % symb_entz
    hgnc_std_map( Entz, Hgnc, UrlF, Csv, StdO, SrcUrl/DnDt, EntzHgncF ),               % entz_hgnc
    hgnc_std_map( Hgnc, Entz, UrlF, Csv, StdO, SrcUrl/DnDt, HgncEntzF ),               % hgnc_entz
    hgnc_std_map( Ensg, Hgnc, UrlF, Csv, StdO, SrcUrl/DnDt, EnsgF ),      % ensg_hgnc
    hgnc_std_map( Hgnc, Ensg, UrlF, Csv, StdO, SrcUrl/DnDt, HgncEnsgF ),      % map_hgnc_hgnc_ensg
    hgnc_std_map( Hgnc, Chrm, UrlF, Csv, StdO, SrcUrl/DnDt, ChrmF ),          % e
    hgnc_std_map( Hgnc, Ccds, UrlF, Csv, StdO, SrcUrl/DnDt, CcdsF ),          % 
    hgnc_std_map( Ccds, Hgnc, UrlF, Csv, StdO, SrcUrl/DnDt, HcdsF ),          % 

    debuc( Self, 'doing links...', [] ),
    % Files = [HSf,HNf,SHf,EcHf,EcSf,ESf,SEf,EnSf,HEf,HEnf,HEcf,HNcf,NcHf, SynoF,PrevF,ChrmF, CcdsF,HcdsF ],
    Files = [HgncF,SynoF,PrevF,HgncNameF,SymbF,EntzF,SymbEntzF,HgncEntzF,EntzHgncF,EnsgF,HgncEnsgF,ChrmF,CcdsF,HcdsF],
    maplist( link_to_bio_sub(hgnc), Files ),
    % file_name_extension( TxtF, gz, GzF ),
    % delete_file( TxtF ),
    working_directory( _, Old ).

hgnc_std_map( Cid1, Cid2, CsvF, Csv, StdO, SrcUrl/DnDt, OutF ) :-
    hgnc_std_column_to_value_call( Cid1, Call1 ),
    hgnc_std_column_to_value_call( Cid2, Call2 ),
    Opts = [to_value_1(Call1),to_value_2(Call2),prefix(hgnc)|StdO],
    % debuc( std_maps_hgnc, 'doing file for columns: ~w, ~w', [Cid1, Cid2] ),
    csv_ids_map( CsvF, Cid1, Cid2, Csv, OutF, [source(SrcUrl),datetime(DnDt)|Opts] ),
    debuc( std_maps_hgnc, 'deposited on: ~w (columns: ~w, ~w)', [OutF,Cid1,Cid2] ).

hgnc_std_column_to_value_call( 'HGNC ID', de_semi('HGNC') ).  % old
hgnc_std_column_to_value_call( 'hgnc_id', de_semi('HGNC') ).
hgnc_std_column_to_value_call( 'Approved Symbol', non_empty_atom ). % old
hgnc_std_column_to_value_call( 'symbol', non_empty_atom ).
hgnc_std_column_to_value_call( 'alias_symbol', sep_split('|') ).
hgnc_std_column_to_value_call( 'prev_symbol', sep_split('|') ).
hgnc_std_column_to_value_call( 'Approved Name', non_empty_atom ). % old
hgnc_std_column_to_value_call( 'name', non_empty_atom ). % old
% hgnc_std_column_to_value_call( 'Entrez Gene ID + supplied by NCBI', pos_integer ).
hgnc_std_column_to_value_call( 'Entrez Gene ID', pos_integer ).  % old
hgnc_std_column_to_value_call( 'entrez_id', pos_integer ).  % old
% hgnc_std_column_to_value_call( 'Entrez Gene ID (supplied by NCBI)', pos_integer ).
% hgnc_std_column_to_value_call( 'Ensembl ID + supplied by Ensembl', non_empty_atom ). 
hgnc_std_column_to_value_call( 'ensembl_gene_id', non_empty_atom ).
hgnc_std_column_to_value_call( 'Chromosome', non_empty_atom ).   % old
hgnc_std_column_to_value_call( 'location', non_empty_atom ).  
hgnc_std_column_to_value_call( 'CCDS IDs', non_empty_atom ).  % old
hgnc_std_column_to_value_call( 'ccds_id', non_empty_atom ). 
      % fixme: prefixed ENSG
    
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
    !,
    Numb > 0.
pos_integer( Atom, Numb ) :-
     atom_number( Atom, Numb ),
     !,
     integer( Numb ), 
     Numb > 0.

non_empty_atom( Other, NonEmpty ) :-
    Other \== '',
    NonEmpty = Other.
