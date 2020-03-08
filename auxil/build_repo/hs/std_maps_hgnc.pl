
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
:- lib(link_to_bio_sub/2).  % link_to_map_sub/2
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2

:- debug(std_maps_hgnc). % fixme:
:- debug(hgnc).

true(_,_).

% Good on 2017/03/03.
% Comprehensive overhaul, as location of source file its contents and header names, had changed...
%    see src/std_hs-19.02.08.old.pl for the old version
%
std_maps_hgnc_defaults( Defs ) :-
    % absolute_file_name( bio_db_build_downloads(hgnc), Dir ),
    % expand_file_name( '$local/../work/db/data/hgnc', [Exp] ),
    % absolute_file_name( Exp, Dir ),
    Defs = [ debug(true), download(true) ].

% std_maps_hgnc( +Opts ).
%
% Create some maps from HGNC's "complete" data file.
%
% Opts
% * dir(Dir=maps)      sub-directory for creating the maps
% * download(Dn=true)  set to false to skip downloading a fresh copy of the HGNC file(s)
% * map_prefix(Mfx)    if present is passed on csv_ids_map/6, else their default applies
%
%==
%  ?- std_maps_hgnc.
%  ?- cd( '$local/../work/db/maps/hgnc' ).
%  ?- shell( 'wc -l hgnc_id*' ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
% @version  0.2 2015/3/18,   added db based prefix
% @version  0.3 2019/2/8,    accommodate the changes to the location and format of the file at the source
% @tbd convert to url_..._mirror.pl
%
std_maps_hgnc :-
    std_maps_hgnc( [] ).

std_maps_hgnc( Args ) :-
    CsvF = 'hgnc_complete_set.txt',
    Self = std_maps_hgnc,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    absolute_file_name( bio_db_build_downloads(hgnc), Dir ),
    os_make_path( Dir, debug(true) ),
    hgnc_download_file( SrcUrl, [dir(Dir)|Opts] ),
    working_directory( Old, Dir ),
    % HgncTxtF = 'hgnc_complete_set.txt',
    % GzF = 'hgnc_complete_set.txt.gz',
    % @ gunzip(-f,-k, GzF ),
    % bio_db_dnt_times( 'hgnc_complete_set.txt.gz.dnt', DnDt, _DnEnd ),
    bio_db_dnt_times( 'hgnc_complete_set.txt.dnt', DnDt, _DnEnd ),

    % CsvF = '14.07.02-hgnc_complete_set.tsv',
    SubDir = maps,
    % fixme: delete SubDir?  through option ?
    make_directory_path( SubDir ),

    options_propagate( map_prefix, Opts, StdOT, true ),
    StdO= [dir(SubDir),cnm_transform(hgnc_cname)|StdOT],
    Entz = 'entrez_id',
    Ensg = 'ensembl_gene_id',
    Hgnc = 'hgnc_id',
    Symb = 'symbol',
    Name = 'name',
    Prev = 'prev_symbol',
    Syno = 'alias_symbol',
    Chrm = 'location',
    Ccds =  'ccds_id',

    csv_ids_rows( CsvF, '\t', Csv ),
    hgnc_std_map( Hgnc, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, HgncF ),               % hgnc_symb

    % fixme allow hgnc_std_map's called predicate to deal with multiple entries from single row
    % also it needs to be told not to sort some maps (but with ability to check uniqueness
    % hgnc_extra_symbols_column( Csv, 'alias_symbol', map_hgnc_syno_symb, SrcUrl/DnDt, SynoF ),
    % hgnc_extra_symbols_column( Csv, 'prev_symbol', map_hgnc_prev_symb, SrcUrl/DnDt, PrevF ),
    hgnc_std_map( Syno, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, SynoF ),               % hgnc_name
    hgnc_std_map( Prev, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, PrevF ),               % hgnc_name

    hgnc_std_map( Hgnc, Name, CsvF, Csv, StdO, SrcUrl/DnDt, HgncNameF ),               % hgnc_name
    hgnc_std_map( Symb, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, SymbF ),               % symb_hgnc
    hgnc_std_map( Entz, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, EntzF ),               % entz_symb
    hgnc_std_map( Symb, Entz, CsvF, Csv, StdO, SrcUrl/DnDt, SymbEntzF ),               % symb_entz
    hgnc_std_map( Entz, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, EntzHgncF ),               % entz_hgnc
    hgnc_std_map( Hgnc, Entz, CsvF, Csv, StdO, SrcUrl/DnDt, HgncEntzF ),               % hgnc_entz
    hgnc_std_map( Ensg, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, EnsgF ),      % ensg_hgnc
    hgnc_std_map( Hgnc, Ensg, CsvF, Csv, StdO, SrcUrl/DnDt, HgncEnsgF ),      % map_hgnc_hgnc_ensg
    hgnc_std_map( Hgnc, Chrm, CsvF, Csv, StdO, SrcUrl/DnDt, ChrmF ),          % e
    hgnc_std_map( Hgnc, Ccds, CsvF, Csv, StdO, SrcUrl/DnDt, CcdsF ),          % 
    hgnc_std_map( Ccds, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, HcdsF ),          % 

    debug( std_maps_hgnc, 'doing links...', [] ),
    debug( link_to_map_sub ),
    % Files = [HSf,HNf,SHf,EcHf,EcSf,ESf,SEf,EnSf,HEf,HEnf,HEcf,HNcf,NcHf, SynoF,PrevF,ChrmF, CcdsF,HcdsF ],
    Files = [HgncF,SynoF,PrevF,HgncNameF,SymbF,EntzF,SymbEntzF,HgncEntzF,EntzHgncF,EnsgF,HgncEnsgF,ChrmF,CcdsF,HcdsF],
    maplist( link_to_map_sub(hgnc), Files ),
    % file_name_extension( TxtF, gz, GzF ),
    % delete_file( TxtF ),
    working_directory( _, Old ).

hgnc_extra_symbols_column( Csv, Cnm, Stem, SrcUrl/DnDt, ExtrF ) :-
    % memberchk( 'Synonyms'=Synonyms, Csv ),
    % Cnm2 = 'Approved Symbol',
    Cnm2 = 'symbol',
    memberchk( Cnm2=ApvSymbs, Csv ),
    memberchk( Cnm=ExtSymbs, Csv ),
    Term =.. [Stem,Syno,ApvSymb],
    findall( Term,      ( nth1(N,ApvSymbs,ApvSymb),nth1(N,ExtSymbs,NSynonyms),
                          at_con( Synos, ', ', NSynonyms ),
                          member(Syno,Synos),
                          Syno\==ApvSymb,
                          Syno\==''
                        ),
                                SynoClauses ),
    os_dir_stem_ext( maps, Stem, pl, ExtrF ),
    % SynoF = 'maps/syno_symb.pl',  % fixme, for links this might have to be sybo_symb.csv
    % csv_write_file( SynoF, [row('Synonym','HGNC Symbol')|SynoRows] ),
    portray_clauses( SynoClauses, file(ExtrF) ),
    debug( std_maps_hgnc, 'Wrote file: ~p', ExtrF ),
    TermOpts = [header(Cnm,Cnm2),source(SrcUrl),datetime(DnDt)],
    bio_db_add_infos_to( TermOpts, ExtrF ).

hgnc_std_map( Cid1, Cid2, CsvF, Csv, StdO, SrcUrl/DnDt, OutF ) :-
    hgnc_std_column_to_value_call( Cid1, Call1 ),
    hgnc_std_column_to_value_call( Cid2, Call2 ),
    Opts = [to_value_1(Call1),to_value_2(Call2),prefix(hgnc)|StdO],
    debug( std_maps_hgnc, 'doing file for columns: ~w, ~w', [Cid1, Cid2] ),
    csv_ids_map( CsvF, Cid1, Cid2, Csv, OutF, [source(SrcUrl),datetime(DnDt)|Opts] ),
    debug( std_maps_hgnc, 'deposited on: ~w', OutF ).

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
    Numb > 0.

non_empty_atom( Other, NonEmpty ) :-
    Other \== '',
    NonEmpty = Other.

hgnc_cname( A, B ) :-
    hgnc_cname_known( A, B ),
    !.
hgnc_cname( A, A ).

% cmpl = complement = both currated by HGNC and supplied by respective database.
% 
hgnc_cname_known( 'HGNC ID', hgnc ).
hgnc_cname_known( 'hgnc_id', hgnc ).
% hgnc_cname_known( 'Entrez Gene ID (supplied by NCBI)', 'entz-ncbi' ).
hgnc_cname_known( 'Entrez Gene ID', 'entz-appv' ).
hgnc_cname_known( 'entrez_id', 'entz' ).
% hgnc_cname_known( 'Entrez Gene ID + supplied by NCBI', entz ).  % was entz_cmpl
hgnc_cname_known( 'Ensembl ID + supplied by Ensembl', ensg ). % was ensg_cmpl
hgnc_cname_known( 'ensembl_gene_id', ensg ). % was ensg_cmpl
hgnc_cname_known( 'Approved Symbol', symb ).
hgnc_cname_known( 'symbol', symb ).
hgnc_cname_known( 'Approved Name', name ).
hgnc_cname_known( 'name', name ).
hgnc_cname_known( 'prev_symbol', prev ).
hgnc_cname_known( 'alias_symbol', syno ).
hgnc_cname_known( 'Chromosome', chrb ).  % chromosome base eg 2p24.1  % old
hgnc_cname_known( 'location', chrb ).  % chromosome base eg 2p24.1
hgnc_cname_known( 'CCDS IDs', ccds ).  % 
hgnc_cname_known( 'ccds_id', ccds ).  % 

hgnc_download_file( Ftp, Opts ) :-
    options( download(DnloadB), Opts ),
    debug( std_maps_hgnc, 'Download boolean: ~w', DnloadB ),
    hgnc_boolean_download_file( DnloadB, Ftp, Opts ).

hgnc_boolean_download_file( false, Ftp, _Opts ) :-
    % Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz'.
    Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt'.
hgnc_boolean_download_file( true, Ftp, Opts ) :-
    debug( url_local ),
    memberchk( dir(Dir), Opts ),
    % Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz',
    Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt',
    url_file_local_date_mirror( Ftp, Dir, [date(prefix),interface(wget)] ).
