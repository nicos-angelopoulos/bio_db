
% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(bio_db).
:- lib(os_lib).
:- lib(by_unix).
:- lib(options).
:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:portray_clauses/2).

% also sets lib alias that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(de_semi/3).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/2).  % link_to_map_sub/2
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2

:- debug(std_maps_hgnc). % fixme:
:- debug(hgnc).

true(_,_).

% Good on 2017/03/03.
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
%==
%  ?- std_maps_hgnc.
%  ?- cd( '$local/../work/db/maps/hgnc' ).
%  ?- shell( 'wc -l hgnc_id*' ).
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
% @version  0.2 2015/3/18,  added db based prefix
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
	GzF = 'hgnc_complete_set.txt.gz',
	@ gunzip(-f,-k, GzF ),
	bio_db_dnt_times( 'hgnc_complete_set.txt.gz.dnt', DnDt, _DnEnd ),

	% CsvF = '14.07.02-hgnc_complete_set.tsv',
	SubDir = maps,
	% fixme: delete SubDir?  through option ?
	make_directory_path( SubDir ),

	options_propagate( map_prefix, Opts, StdOT, true ),
	StdO= [dir(SubDir),cnm_transform(hgnc_cname)|StdOT],
	Entz = 'Entrez Gene ID',
	Hgnc = 'HGNC ID',
	Symb = 'Approved Symbol',
	Name = 'Approved Name',
	EntzNcbi = 'Entrez Gene ID (supplied by NCBI)',
	EntzCmpl = 'Entrez Gene ID + supplied by NCBI',
	EnsgCmpl = 'Ensembl ID + supplied by Ensembl',
	Chrm = 'Chromosome',
	Ccds =  'CCDS IDs',

	% csv_read_file( CsvF, Csv, [
	csv_ids_rows( CsvF, '\t', Csv ),

	memberchk( Entz=GeneIDs, Csv ),
	memberchk( EntzNcbi=NCBIGeneIDs, Csv ),
	maplist( cohese_gene_id, GeneIDs, NCBIGeneIDs, CohGids ),
	EntzCsv = [EntzCmpl=CohGids|Csv],

	memberchk( 'Ensembl Gene ID'=EnsGs, Csv ),
	memberchk( 'Ensembl ID (supplied by Ensembl)'=NcbiEnsGs, Csv ),
	maplist( cohese_ensebl_gene_id, EnsGs, NcbiEnsGs, CohEnsGs ),
	EnsgCsv = [EnsgCmpl=CohEnsGs|Csv], 

	hgnc_std_map( Hgnc, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, HSf ),               % hgnc_symb

	% fixme allow hgnc_std_map's called predicate to deal with multiple entries from single row
	% also it needs to be told not to sort some maps (but with ability to check uniqueness
	hgnc_extra_symbols_column( Csv, 'Synonyms', map_hgnc_syno_symb, SrcUrl/DnDt, SynoF ),
	hgnc_extra_symbols_column( Csv, 'Previous Symbols', map_hgnc_prev_symb, SrcUrl/DnDt, PrevF ),

	hgnc_std_map( Hgnc, Name, CsvF, Csv, StdO, SrcUrl/DnDt, HNf ),               % hgnc_name
	hgnc_std_map( Symb, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, SHf ),               % symb_hgnc
	hgnc_std_map( EntzCmpl, Hgnc, CsvF, EntzCsv, StdO, SrcUrl/DnDt, EcHf ),      % entz_cmpl_symb    % entz_cmpl_hgnc ? 
	hgnc_std_map( EntzCmpl, Symb, CsvF, EntzCsv, StdO, SrcUrl/DnDt, EcSf ),      % entz_cmpl_symb    % entz_cmpl_hgnc ? 
	hgnc_std_map( Entz, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, ESf ),               % entz_symb
	hgnc_std_map( EntzNcbi, Symb, CsvF, Csv, StdO, SrcUrl/DnDt, EnSf ),          % entz_ncbi_symb
	hgnc_std_map( Hgnc, Entz, CsvF, Csv, StdO, SrcUrl/DnDt, HEf ),               % hgnc_entz
	hgnc_std_map( Hgnc, EntzNcbi, CsvF, Csv, StdO, SrcUrl/DnDt, HEnf ),          % hgnc_entz_ncbi
	hgnc_std_map( Hgnc, EntzCmpl, CsvF, EntzCsv, StdO, SrcUrl/DnDt, HEcf ),      % hgnc_entz_cmpl
	hgnc_std_map( Symb, EntzCmpl, CsvF, EntzCsv, StdO, SrcUrl/DnDt, SEf ),       % symb_entz_cmpl
	hgnc_std_map( Hgnc, EnsgCmpl, CsvF, EnsgCsv, StdO, SrcUrl/DnDt, HNcf ),      % hgnc_ensg_cmpl
	hgnc_std_map( EnsgCmpl, Hgnc, CsvF, EnsgCsv, StdO, SrcUrl/DnDt, NcHf ),      % ensg_cmpl_hgnc
	hgnc_std_map( Hgnc, Chrm, CsvF, Csv, StdO, SrcUrl/DnDt, ChrmF ),      	  % ensg_cmpl_hgnc
	hgnc_std_map( Hgnc, Ccds, CsvF, Csv, StdO, SrcUrl/DnDt, CcdsF ),      	  % 
	hgnc_std_map( Ccds, Hgnc, CsvF, Csv, StdO, SrcUrl/DnDt, HcdsF ),      	  % 

	debug( std_maps_hgnc, 'doing links...', [] ),
	debug( link_to_map_sub ),
	Files = [HSf,HNf,SHf,EcHf,EcSf,ESf,SEf,EnSf,HEf,HEnf,HEcf,HNcf,NcHf, SynoF,PrevF,ChrmF, CcdsF,HcdsF ],
	maplist( link_to_map_sub(hgnc), Files ),
	file_name_extension( TxtF, gz, GzF ),
	delete_file( TxtF ),
	working_directory( _, Old ).

hgnc_extra_symbols_column( Csv, Cnm, Stem, SrcUrl/DnDt, ExtrF ) :-
	% memberchk( 'Synonyms'=Synonyms, Csv ),
	Cnm2 = 'Approved Symbol',
	memberchk( Cnm2=ApvSymbs, Csv ),
	memberchk( Cnm=ExtSymbs, Csv ),
	Term =.. [Stem,Syno,ApvSymb],
	findall( Term,		( nth1(N,ApvSymbs,ApvSymb),nth1(N,ExtSymbs,NSynonyms),
							bio_db:at_con( Synos, ', ', NSynonyms ),
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

hgnc_std_column_to_value_call( 'HGNC ID', de_semi('HGNC') ).
hgnc_std_column_to_value_call( 'Approved Symbol', non_empty_atom ).
hgnc_std_column_to_value_call( 'Approved Name', non_empty_atom ).
hgnc_std_column_to_value_call( 'Entrez Gene ID + supplied by NCBI', pos_integer ).
hgnc_std_column_to_value_call( 'Entrez Gene ID', pos_integer ).
hgnc_std_column_to_value_call( 'Entrez Gene ID (supplied by NCBI)', pos_integer ).
hgnc_std_column_to_value_call( 'Ensembl ID + supplied by Ensembl', non_empty_atom ). 
hgnc_std_column_to_value_call( 'Chromosome', non_empty_atom ). 
hgnc_std_column_to_value_call( 'CCDS IDs', non_empty_atom ). 
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
hgnc_cname_known( 'Entrez Gene ID (supplied by NCBI)', 'entz-ncbi' ).
hgnc_cname_known( 'Entrez Gene ID', 'entz-appv' ).
hgnc_cname_known( 'Entrez Gene ID + supplied by NCBI', entz ).  % was entz_cmpl
hgnc_cname_known( 'Ensembl ID + supplied by Ensembl', ensg ). % was ensg_cmpl
hgnc_cname_known( 'Approved Symbol', symb ).
hgnc_cname_known( 'Approved Name', name ).
hgnc_cname_known( 'Chromosome', chrb ).  % chromosome base eg 2p24.1
hgnc_cname_known( 'CCDS IDs', ccds ).  % 

hgnc_download_file( Ftp, Opts ) :-
	options( download(DnloadB), Opts ),
	debug( std_maps_hgnc, 'Download boolean: ~w', DnloadB ),
	hgnc_boolean_download_file( DnloadB, Ftp, Opts ).

hgnc_boolean_download_file( false, Ftp, _Opts ) :-
	Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz'.
hgnc_boolean_download_file( true, Ftp, Opts ) :-
	debug( url_local ),
	memberchk( dir(Dir), Opts ),
	Ftp = 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz',
	url_file_local_date_mirror( Ftp, Dir, date(prefix) ).
