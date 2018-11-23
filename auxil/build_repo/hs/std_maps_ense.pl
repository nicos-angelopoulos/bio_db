
:- set_prolog_flag(stack_limit, 2 000 000 000).

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module( library(lib) ).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib(stoics_lib:kv_decompose/3).

% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% load necessary data that has already been generated
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_ensg_hgnc')).
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb')).
% :- ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')).

% local libs & sources
:- lib(csv_to_pl/1).
:- lib(link_to_bio_sub/2).
:- lib(bio_db_dnt_times/3).
:- lib(url_file_local_date_mirror/3).
:- lib(bio_db_add_infos/1).   % bio_db_add_infos_to/2

:- debug(std_maps_ense).

std_maps_ense_defaults(debug(true)).

/** std_maps_ense.

Maps based on ensembl .gtf file. Including transcripts to genes, genes
to hgncs and locations of transcipt and genes to chromosomome locations.

Nonmeclature:
    * ense: the database abbv.
    * enst: ensembl transcript
    * ensg: ensembl gene
    * chrl: chromosomal location

Produces:
	map_ense_enst_ensg
	map_ense_ensg_hgnc
	map_ense_ensg_symb
	map_ense_ensg_chrl
	map_ense_enst_chrl

@author  nicos angelopoulos
@version 0.1, 2016/6/17
@see ftp://ftp.ensembl.org/pub/release-84/gtf/homo_sapiens/Homo_sapiens.GRCh38.84.gtf.gz
@see http://ftp.ensembl.org/pub/current_gtf/homo_sapiens/
@tbd automate selection of latest version
*/
std_maps_ense( Args ) :-
    Self = std_maps_ense,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    % load necessary data that has already been generated
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_ensg_hgnc')),
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_hgnc_symb')),
    ensure_loaded(hgnc:bio_db_build_downloads('hgnc/maps/map_hgnc_symb_hgnc')),

	debug( Self, 'Starting...', true ),
	absolute_file_name( bio_db_build_downloads(ense), DnDir ),
	os_make_path( DnDir ),
	debug( Self, 'Downloads dir for ense: ~p', DnDir ),
	% Url = 'ftp://ftp.ensembl.org/pub/release-85/gtf/homo_sapiens/Homo_sapiens.GRCh38.85.gtf.gz',
	Url = 'ftp://ftp.ensembl.org/pub/release-89/gtf/homo_sapiens/Homo_sapiens.GRCh38.89.gtf.gz',

	url_file_local_date_mirror( Url, DnDir, file(File) ),
	debug( Self, 'Dnload done, file is: ~p', File ),
	working_directory( Old, DnDir ),
	bio_db_dnt_times( File, DnDt, _DnEn ),
	debug( by_unix ),
	os_un_zip( File, _, [keep(true),on_exists(skip),debug(true)] ),
	% @ gunzip( -k, -f, File ),
	os_ext( gz, Stem, File ),
	os_ext( tab, Stem, TabF ),
	% fixme: ???:
	% Stem = 'Homo_sapiens.GRCh38.84.gtf-16.06.20', 
	% TabF = 'Homo_sapiens.GRCh38.84.gtf-16.06.20.tab', 
	atomic_list_concat( [grep,'-v','"^#"',Stem,'>',TabF], ' ', Shell ),
	write( shelling(Shell) ), nl,
	shell( Shell ),
	debug( Self, '...done...', true ),
	% @ grep( -v, '"^#"', Stem, '>', TabF ),
	@ ls(),
	mtx( TabF, Rows, sep(tab) ),
	debug_call( Self, length, rows/Rows ),
	ense_transcripts( Rows, EnsTGRows, EnsTLRows ),
	mtx( 'map_ense_enst_ensg.csv', EnsTGRows ),
	mtx( 'map_ense_enst_chrl.csv', EnsTLRows ),

	ense_genes( Rows, EnsGHRows, EnsGSRows, EnsGCRows ),
	mtx( 'map_ense_ensg_hgnc.csv', EnsGHRows ),
	mtx( 'map_ense_ensg_symb.csv', EnsGSRows ),
	mtx( 'map_ense_ensg_chrl.csv', EnsGCRows ),

	Csvs = [ 'map_ense_enst_ensg.csv', 'map_ense_enst_chrl.csv',
		    'map_ense_ensg_hgnc.csv', 'map_ense_ensg_symb.csv',
              'map_ense_ensg_chrl.csv'
	       ],
	debug( Self, 'mapping: ~w', [Csvs] ),
	maplist( csv_to_pl, Csvs ),

	/*

	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,
	findall( row(EnsG,ChrG,SrtG,EndG,DirG),  (
										member(RowG,Rows),


	*/
	maplist( new_ext(pl), Csvs, Pls ),
	% fixme: do the headers :( 
	AddOpts = [source(Url),datetime(DnDt)],
	% maplist( bio_db_add_infos_to( [header(row('GO Term','HGNC Symbol'))|AddOpts], 'maps/map_gont_gont_symb.pl' ),
	% maplist( bio_db_add_infos_to(AddOpts), Pls ),
	Headers = [	
		row('Ensembl Transcript','Ensembl Gene'),
		row('Ensembl Transcript','Chromosome', 'Start', 'End', 'Direction'),
		row('Ensembl Gene','HGNC ID'),
		row('Ensembl Gene','HGNC Symbol'),
		row('Ensembl Gene','Chromosome', 'Start', 'End', 'Direction')
			],
	findall( PlF, (	nth1(N,Pls,PlF),	nth1(N,Headers,Header),
					debug( Self, 'ingoing prolog file: ~p', PlF ),
					debug( Self, '...with header: ~w and options: ~w', [Header,AddOpts] ),
					bio_db_add_infos_to([header(Header)|AddOpts], PlF)
					),
					_PlFs ),
	os_make_path( maps ),
	maplist( mv_to_sub(maps), Pls ),
    @ rm( -f, Stem, TabF ),
	working_directory( _, maps ),
	maplist( link_to_map_sub(ense), Pls ),

	working_directory( _, Old ),
	debug( Self, '...Done', true ).

mv_to_sub( Sub, File ) :-
	os_path( Sub, File, Rel ),
	rename_file( File, Rel ).

new_ext( New, File, NewFile ) :-
	os_ext( _Old, New, File, NewFile ).
	
ense_genes( [], [], [], [] ).
ense_genes( [RowG|Rows], GHRows, GSRows, [EnsGC|GCRows] ) :-
	RowG = row(ChrG,_Db,gene,SrtG,EndG,_,DirG,_,InfoG),
	EnsGC= row(EnsG,ChrG,SrtG,EndG,DirG),
	ense_info( gene_id, InfoG, EnsG ),
	ense_info( gene_name, InfoG, EnsN ),
	ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ),
	!,
	ense_genes( Rows, TGHRows, TGSRows, GCRows ).
ense_genes( [RowG|Rows], _, _, _ ) :-
	RowG = row(_ChrG,_Db,gene,_SrtG,_EndG,_,_DirG,_,_InfoG),
	!,
	length( Rows, Len ),
	throw( tripped_on_gene_row(RowG,Len) ).
ense_genes( [_RowG|Rows], GHRows, GSRows, GCRows ) :-
	ense_genes( Rows, GHRows, GSRows, GCRows ).

ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ) :-
	% map_hgnc_ensg_hgnc( EnsG, Hgnc ),
	findall( AHgnc, hgnc:map_hgnc_ensg_hgnc(EnsG,AHgnc), [Hgnc] ),
        % there are 10 or so EnsGs, map to multi Hs
	!,
	hgnc:map_hgnc_hgnc_symb( Hgnc, Symb ),
	ense_gene_hgnc_symbols( EnsN, Symb ),
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,Symb)|TGSRows].
ense_gene_hgnc( EnsG, EnsN, GHRows, GSRows, TGHRows, TGSRows ) :-
	% \+ map_hgnc_ensg_hgnc( EnsG, _ ),
	hgnc:map_hgnc_symb_hgnc( EnsN, Hgnc ),
	!,  								% fixme: count how many of those we have ...
	GHRows = [row(EnsG,Hgnc)|TGHRows],
	GSRows = [row(EnsG,EnsN)|TGSRows].
ense_gene_hgnc( _EnsG, _EnsN, GHRows, GSRows, GHRows, GSRows ).

ense_gene_hgnc_symbols( Symb, Symb ) :- !.
ense_gene_hgnc_symbols( EnsN, Symb ) :-
	write( ense_gene_symbol_disagreement(EnsN,Symb) ), nl.

ense_transcripts( [], [], [] ).
ense_transcripts( [RowT|Rows], [EnsTG|TGRows], [EnsTL|TLRows] ) :-
	RowT = row(ChrT,_Db,transcript,SrtT,EndT,_,DirT,_,InfoT),
	% findall( row(EnsT,EnsG)-row(EnsT,ChrT,SrtT,EndT,DirT), ( )).
	ense_info( transcript_id, InfoT, EnsT ),
	ense_info( gene_id, InfoT, EnsG ),
	ense_chromosome( ChrT ),
	!,
	EnsTG = row(EnsT,EnsG),
	EnsTL = row(EnsT,ChrT,SrtT,EndT,DirT),
	ense_transcripts( Rows, TGRows, TLRows ).
ense_transcripts( [RowT|Rows], _, _ ) :-
	RowT = row(ChrT,_Db,transcript,_SrtT,_EndT,_,_DirT,_,_InfoT),
	ense_chromosome( ChrT ),
	!,
	length( Rows, Len ),
	throw( tripped_on_transcript_row(RowT,Len) ).
ense_transcripts( [_RowT|Rows], TGRows, TLRows ) :-
	ense_transcripts( Rows, TGRows, TLRows ).

ense_info( Key, Lookup, Value ) :-
	ense_info( Key, Lookup, true, Value ).
	
ense_info( Key, Lookup, _Strict, Value ) :-
	atomic_list_concat( Parts, '"; ', Lookup ),
	atom_concat( Key, ' "', Left ),
	member( Part, Parts ),
	atom_concat( Left, Value, Part ),
	!.
ense_info( Key, Lookup, Strict, false ) :-
	ense_info_failure( Strict, Key, Lookup ).

ense_info_failure( true, Key, Lookup ) :-
	throw( lookup_failure(Key,Lookup) ).
ense_info_failure( false, _Key, _Lookup ).

ense_chromosome( 'X' ) :- !.
ense_chromosome( 'Y' ) :- !.
ense_chromosome( N ) :- integer( N ), !.
